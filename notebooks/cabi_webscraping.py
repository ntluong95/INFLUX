from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.edge.options import Options
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from bs4 import BeautifulSoup

# Configure Selenium WebDriver for Edge
options = Options()
options.use_chromium = True
options.headless = True  # Set to False if you want to see the browser window
driver = webdriver.Edge(options=options)    

# Open the login page
login_url = 'https://ezp.sub.su.se/login?url=https://www.cabidigitallibrary.org/journal/cabicompendium'
driver.get(login_url)


# Wait for the username and password elements to be present
wait = WebDriverWait(driver, 10)  # Wait for up to 10 seconds
username_input = wait.until(EC.presence_of_element_located((By.ID, 'username')))
password_input = wait.until(EC.presence_of_element_located((By.ID, 'password')))  # Adjust the ID according to actual password field ID

# Enter login details
username_input.send_keys("egpe5386")
password_input.send_keys("INFLUXProject2024")


# Locate and click the login button
login_button = wait.until(EC.element_to_be_clickable((By.NAME, '_eventId_proceed')))
login_button.click()

# # After login, wait for the cookie acceptance button to appear and click it
# cookie_accept_button = wait.until(EC.element_to_be_clickable((By.ID, 'onetrust-accept-btn-handler')))
# cookie_accept_button.click()

# Navigate to the specific page
target_url = 'https://www.cabidigitallibrary.org/action/doSearch?SeriesKey=cabicompendium&startPage=0&sortBy=EPubDate'
driver.get(target_url)

# Wait for the page to load, adjust the waiting condition based on specific elements you expect to be loaded
wait.until(EC.presence_of_element_located((By.CSS_SELECTOR, 'div.some-class')))  # Adjust selector to your needs

# Now use BeautifulSoup to parse the page source
soup = BeautifulSoup(driver.page_source, 'html.parser')

# Example of finding a specific element, adjust based on your actual data extraction needs
results = soup.find_all('div', class_='specific-class')  # Adjust this to match the HTML you're interested in

# Print or process your results
print(results)

# Close the driver
driver.quit()

