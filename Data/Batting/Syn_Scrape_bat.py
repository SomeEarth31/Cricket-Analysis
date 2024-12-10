from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.chrome.service import Service
from selenium.webdriver.chrome.options import Options
from webdriver_manager.chrome import ChromeDriverManager
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC

import time

import pandas as pd

from rpy2.robjects import pandas2ri
from rpy2.robjects import r

import sys

url = sys.argv[1]
ind = int(sys.argv[2])  # Convert to integer

# Activate the automatic conversion between pandas and R data frames
pandas2ri.activate()

# Step 1: Set up Chrome options to load the uBlock Origin extension
chrome_options = Options()
chrome_options.add_argument(f"--load-extension={r"C:\Users\samar\AppData\Local\Google\Chrome\User Data\Default\Extensions\ddkjiahejlhfcafbddmgiahcphecmpfh\2024.10.14.189_0"}")  # Update the path accordingly
chrome_options.add_argument('--headless')

driver = webdriver.Chrome(service=Service(ChromeDriverManager().install()), options = chrome_options)
#url = 'https://www.espncricinfo.com/cricketers/ravindra-jadeja-234675/bowling-batting-stats'
driver.get(url)

# Step 3: Allow the page to load
time.sleep(3)

# Step 3.5: Wait for the "Batting" element to become visible
try:
    WebDriverWait(driver, 10).until(
        EC.visibility_of_element_located((By.XPATH, "//span[text()='Bowling']"))
    )
except Exception as e:
    print("Batting element not visible:", e)

# Step 3.75: Check if the "Not Now" button is present and click it
try:
    not_now_button = driver.find_element(By.XPATH, "//button[text()='Not Now']")
    not_now_button.click()
except Exception as e:
    print("No ad/modal appeared or could not click:", e)

# Step 4: Click on the "Batting" element to reveal the dropdown
batting_element = driver.find_element(By.XPATH, "//span[text()='Bowling']")
batting_element.click()
time.sleep(2)

# Step 5: Click on the "Bowling" option in the dropdown
bowling_option = driver.find_element(By.XPATH, "//span[text()='Batting']")
bowling_option.click()

# Step 6: Allow time for the page to update
time.sleep(10)

# Step 7: Check if the "Not Now" button is present and click it
try:
    not_now_button = driver.find_element(By.XPATH, "//button[text()='Not Now']")
    not_now_button.click()
except Exception as e:
    print("No ad/modal appeared or could not click:", e)

# Step 8: Get the updated page source
html = driver.page_source

#Step 9: Get tables
tables2 = pd.read_html(html)

#Step 10: Save as RData
r.assign("tables", tables2)
r(f'save(tables, file="{ind}.RData")')

#Step 11: Close Driver
driver.quit()