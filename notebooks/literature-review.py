
# %%
import os
import csv
import json
from typing import List, Dict
from Bio import Entrez
from Bio import Medline
from transformers import pipeline
from sentence_transformers import SentenceTransformer
import faiss
import spacy
import numpy as np

# %%  
def search_pubmed(query: str, email: str, retmax: int = 1000) -> List[str]:
    """
    Searches PubMed database for the given query and returns a list of IDs.
    
    Parameters:
        query (str): The search query string.
        email (str): The email address associated with the search.
        retmax (int): The maximum number of results to retrieve. Default is 1000.
    
    Returns:
        List[str]: A list of PubMed IDs matching the query.
    """

    Entrez.email = email
    id_list = []
    batch_size = 50000

    for start in range(0, retmax, batch_size):
        handle = Entrez.esearch(db="pubmed", term=query, retstart=start, retmax=batch_size)
        record = Entrez.read(handle)
        handle.close()
        id_list.extend(record["IdList"])

        if len(record["IdList"]) < batch_size:
            break

    return id_list

  

def fetch_details(id_list: List[str], batch_size: int = 10000) -> List[Dict[str, str]]:
    """
    Fetches details for a list of PubMed IDs and returns a list of dictionaries containing the details.
    
    Parameters:
        id_list (List[str]): List of PubMed IDs to fetch details for.
        batch_size (int): The size of each batch for fetching details. Default is 10000.
    
    Returns:
        List[Dict[str, str]]: A list of dictionaries containing the details for each PubMed ID.
    """

    details = []

    for i in range(0, len(id_list), batch_size):

        batch = id_list[i:i+batch_size]
        handle = Entrez.efetch(db="pubmed", id=batch, rettype="medline", retmode="text")
        records = Medline.parse(handle)

        for record in records:
            pubmed_id = record.get("PMID", "")
            title = record.get("TI", "")
            abstract = record.get("AB", "")
            authors = ", ".join(record.get("AU", []))
            journal = record.get("JT", "")
            year = record.get("DP", "")[:4]
            volume = record.get("VI", "")
            issue = record.get("IP", "")
            pages = record.get("PG", "")
            doi = record.get("AID", [""])[0].split(" ")[0] if record.get("AID") else ""
            details.append({

                "pubmed_id": pubmed_id,
                "title": title,
                "abstract": abstract,
                "authors": authors,
                "journal": journal,
                "year": year,
                "volume": volume,
                "issue": issue,
                "pages": pages,
                "doi": doi})

        handle.close()

    return details

  

def export_to_csv(details: List[Dict[str, str]], output_file: str) -> None:
    """
    Exports the details to a CSV file.

    Parameters:
        details (List[Dict[str, str]]): List of dictionaries containing details to be exported.
        output_file (str): The path to the output CSV file.

    Returns:
        None
    """

    with open(output_file, "w", encoding="utf-8", newline="") as file:
        fieldnames = ["PubMed ID", "Title", "Abstract", "Authors", "Journal", "Year", "Volume", "Issue", "Pages", "DOI"]
        writer = csv.DictWriter(file, fieldnames=fieldnames)
        writer.writeheader()
        for detail in details:
            writer.writerow({
                "PubMed ID": detail["pubmed_id"],
                "Title": detail["title"].replace(",", ""),
                "Abstract": detail["abstract"].replace(",", ""),
                "Authors": detail["authors"].replace(",", ";"),
                "Journal": detail["journal"].replace(",", ""),
                "Year": detail["year"],
                "Volume": detail["volume"],
                "Issue": detail["issue"],
                "Pages": detail["pages"],
                "DOI": detail["doi"]
            })

def generate_summary(details: List[Dict[str, str]], query: str) -> str:
    """
    Generates a summary of abstracts by concatenating the abstracts from a list of dictionaries containing details.
    
    Args:
        details (List[Dict[str, str]]): A list of dictionaries containing details with each detail containing the following keys:
            - "abstract" (str): The abstract text.
        query (str): The query string to search for in the abstracts.
    
    Returns:
        str: The generated summary of abstracts.
    """

    # Load pre-trained models
    summarizer = pipeline("summarization", model="facebook/bart-large-cnn")
 
    # Concatenate abstracts using efficient memory handling
    concatenated_abstracts = " ".join([detail["abstract"] for detail in details if detail["abstract"]])

  

    # Generate summary in chunks to avoid memory overflow
    max_chunk_size = 1024
    summary_parts = []

    for i in range(0, len(concatenated_abstracts), max_chunk_size):
        chunk = concatenated_abstracts[i:i + max_chunk_size]
        if chunk:
            summary_parts.append(summarizer(chunk, max_length=150, min_length=50, do_sample=False)[0]['summary_text'])

    summary = ' '.join(summary_parts)

    return summary

  
def export_to_ris(details: List[Dict[str, str]], output_file: str) -> None:
    """
    Export details to a RIS file format.

    Args:
        details (List[Dict[str, str]]): A list of dictionaries containing details to be exported.
        output_file (str): The file path to write the RIS formatted details.

    Returns:
        None
    """

    with open(output_file, "w", encoding="utf-8") as file:
        for detail in details:
            file.write("TY  - JOUR\n")
            file.write(f"ID  - {detail['pubmed_id']}\n")
            file.write(f"TI  - {detail['title']}\n")
            file.write(f"AB  - {detail['abstract']}\n")
            for author in detail["authors"].split(", "):
                file.write(f"AU  - {author}\n")

            file.write(f"JO  - {detail['journal']}\n")
            file.write(f"PY  - {detail['year']}\n")
            file.write(f"VL  - {detail['volume']}\n")
            file.write(f"IS  - {detail['issue']}\n")
            file.write(f"SP  - {detail['pages']}\n")
            file.write(f"DO  - {detail['doi']}\n")
            file.write("ER  - \n\n")

  

def export_to_bibtex(details: List[Dict[str, str]], output_file: str) -> None:
	"""
	Export details to a BibTeX file format.
	
	Args:
	    details (List[Dict[str, str]]): A list of dictionaries containing details to be exported.
	    output_file (str): The file path to write the BibTeX formatted details.
	
	Returns:
	    None
	"""

    with open(output_file, "w", encoding="utf-8") as file:
        for detail in details:
            authors = " and ".join(detail["authors"].split(", "))
            file.write("@article{" + detail["pubmed_id"] + ",\n")
            file.write(" author = {" + authors + "},\n")
            file.write(" title = {" + detail["title"] + "},\n")
            file.write(" journal = {" + detail["journal"] + "},\n")
            file.write(" year = {" + detail["year"] + "},\n")
            file.write(" volume = {" + detail["volume"] + "},\n")
            file.write(" number = {" + detail["issue"] + "},\n")
            file.write(" pages = {" + detail["pages"] + "},\n")
            file.write(" doi = {" + detail["doi"] + "},\n")
            file.write(" abstract = {" + detail["abstract"] + "}\n")
            file.write("}\n\n")

  

def main():
	"""
	Main function that orchestrates searching PubMed, fetching details, and exporting results to different formats.
	"""

    email = "ph.ntluong95@gmail.com"  # Replace with your email address

    query = ("(obese OR obesity OR overweight) AND (thyroid autoimmunity OR Hashimoto's thyroiditis OR "
             "Graves' disease OR Graves hyperthyroidism OR hyperthyroidism OR hypothyroidism OR "
             "TPOAb OR TGAb OR thyroid peroxidase antibodies OR thyroid peroxidase antibody OR "
             "thyroglobulin antibodies OR thyroglobulin antibody OR thyroiditis)")

    csv_output_file = "search_results.csv"  # Output CSV file name
    ris_output_file = "search_results.ris"  # Output RIS file name
    bibtex_output_file = "search_results.bib"  # Output BibTeX file name

  

    # Search PubMed

    id_list = search_pubmed(query, email, retmax=10000)
    print(f"Total articles found: {len(id_list)}")

  

    # Fetch details in smaller batches to manage memory
    details = fetch_details(id_list, batch_size=1000)
    print(f"Total articles fetched: {len(details)}")

  

    # Export results to different formats

    export_to_csv(details, csv_output_file)
    export_to_ris(details, ris_output_file)
    export_to_bibtex(details, bibtex_output_file)

  
# %%
if __name__ == "__main__":
    main()