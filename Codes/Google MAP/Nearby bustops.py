import openpyxl
import googlemaps

# Your Google Maps API Key
API_KEY = 'API_KEY'

# Initialize the Google Maps client
gmaps = googlemaps.Client(key=API_KEY)

# Load the Excel file
file_path = r'G:\4-2\CE-400\Google MAP\origin_access.xlsx'
wb = openpyxl.load_workbook(file_path)
sheet = wb.active

# Get the column index for "Origin" (assuming it's the first column)
origin_column = 'A'  # Adjust this if "Origin" is in another column
result_column = 'B'  # We'll add results in column B (or adjust as needed)

# Write a heading for the new column
sheet[f'{result_column}1'] = 'Bus Stop Nearby'

# Iterate over the locations in the Origin column (starting from row 2)
for row in range(2, sheet.max_row + 1):
    origin = sheet[f'{origin_column}{row}'].value

    # Get the lat-long of the origin using the Geocoding API
    geocode_result = gmaps.geocode(origin)

    if geocode_result:
        # Extract the lat-long of the location
        lat_lng = geocode_result[0]['geometry']['location']
        latitude, longitude = lat_lng['lat'], lat_lng['lng']

        # Search for bus stops within a 1000m radius using the Places API
        places_result = gmaps.places_nearby(
            location=(latitude, longitude),
            radius=1000,
            type='bus_station'
        )

        # Check if any bus stops were found
        if places_result['results']:
            sheet[f'{result_column}{row}'] = 1  # Bus stop found
        else:
            sheet[f'{result_column}{row}'] = 0  # No bus stop found
    else:
        sheet[f'{result_column}{row}'] = 0  # No geocode result for the location

# Save the updated Excel file
wb.save(file_path)

print("Process completed successfully!")

