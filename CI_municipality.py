# Load packages
import bs4
import lxml
import requests
import csv

# Set up headers and get initial webpage
headers = {
    'User-Agent': 'Mozilla/5.0 (Macintosh; Intel Mac OS X 11_2_3) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/14.0.3 Safari/605.1.15'}
res = requests.get(
    '''https://www.civicinfo.bc.ca/municipalities''', headers=headers)
res.raise_for_status()

# Parse intial webpage
soup1 = bs4.BeautifulSoup(res.text, 'lxml')

# Count results on page
num_results_str = soup1.find("b").text.split()
num_results = int(num_results_str[0])

# Calculate number of 10 entry pages
num_pages = num_results // 10 
if num_results % 10 != 0:
   num_pages += 1
   
pages = list(range(num_pages+1))[1:]
contacts = []

# Iterate through entries on each page and pull links into a list
for page in pages:
    
    url_num = str(page)
    url = '''https://www.civicinfo.bc.ca/municipalities?pn=''' + url_num
    with requests.Session() as s:
        s.get(url, headers=headers)
        soup = bs4.BeautifulSoup(s.get(url, headers=headers).text, 'lxml')
        contact_list = soup.find("ol")
        contact_links = contact_list.find_all("li")
        for contact in contact_links:
            try:
                link = contact.find("a")
                contacts.append(link['href'])
            except:
                pass

# Open CSV and write column headers      
with open("CivicInfoMunicipalities.csv", 'w', newline='') as csv_file:
    csv_writer = csv.writer(csv_file)
    csv_writer.writerow(["Region", "Regional District", "Community", "Address", "City",
                         "Contact Name", "Title", "Email"])
    
    # Iterate through the list of links
    for site in contacts:
        with requests.Session() as s:

           # Parse the linked page
            soup2 = bs4.BeautifulSoup(s.get('''https://www.civicinfo.bc.ca/''' + site, headers=headers).text, 'lxml')

            # Pull data from page
            wrapper = soup2.select("#content_wrapper")[0]
            content = wrapper.select("#content")[0]
            community = content.find("h2").text.split("Incorporated")[0]
            region_area = content.find("div", class_ = "smBlock")
            region_info  = region_area.find_all("a")
            regional_district = region_info[0].text.split('''\xa0''')[-1]
            region = region_info[-1].text.split('''\xa0''')[-1]
            contact_info = content.find_all("tbody")[0]
            address = contact_info.find_all("td")[0].text.split(": ")[1]
            city = address.split(",")[1].strip()
            staff_info = content.find_all("tbody")[2]
            staff_members = staff_info.find_all("tr")

            # Try to find Economic Development contact info
            title = ""
            email = ""
            contact_name = ""
            for member in staff_members[:-1]:
                if "Economic Development" in member.text:
                    contact_url = '''https://www.civicinfo.bc.ca/''' + member.find("a")["href"]
                    with requests.Session() as r:
                        soup3 = bs4.BeautifulSoup(r.get(contact_url, headers=headers).text, 'lxml')
                        contact_content = soup3.select("#content")[0].find_all("div", attrs={'style':'padding-top: 10px;'})[0]
                        contact_name = contact_content.find("h2").text
                        contact_sections = contact_content.find_all("div")
                        title = contact_sections[1].text.split("Primary Job Title")[1].strip()
                        email = contact_content.find_all("a")[-1].text.split('''\xa0''')[-1]
                    break
                
                if "*" in member.text:
                    contact_url = '''https://www.civicinfo.bc.ca/''' + member.find("a")["href"]        
                    with requests.Session() as r:
                        soup3 = bs4.BeautifulSoup(r.get(contact_url, headers=headers).text, 'lxml')
                        contact_content = soup3.select("#content")[0].find_all("div", attrs={'style':'padding-top: 10px;'})[0]
                        contact_name = contact_content.find("h2").text
                        contact_sections = contact_content.find_all("div")
                        title = contact_sections[1].text.split("Primary Job Title")[1].strip()
                        email = contact_content.find_all("a")[-1].text.split('''\xa0''')[-1]
                    
            # Write data to CSV
            csv_writer.writerow([region, regional_district, community, address, city, contact_name, title, email])