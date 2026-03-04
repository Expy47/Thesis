import pandas as pd
import googlemaps
import time
from datetime import datetime, timedelta

# Replace with your actual Google Maps API key
API_KEY = 'API_KEY'  # Replace with your actual Google Maps API key
gmaps = googlemaps.Client(key=API_KEY)

# Load the Excel file containing origins, destinations, and timestamps
file_path = r'g:\4-2\CE-400\Google MAP\LaL_egress.xlsx'  # Replace with your Excel file path
df = pd.read_excel(file_path)

# Assuming your Excel has three columns: 'Origin', 'Destination', and 'Timestamp'
origins = df['Origin']
destinations = df['Destination']
timestamps = pd.to_datetime(df['Timestamp'], format='%I:%M:%S %p')  # Convert the Timestamp column to datetime format

# Initialize a list to store the results
results = []

# Define bounding box for Dhaka to confine searches
dhaka_bounds = {
    'northeast': {'lat': 23.9000, 'lng': 90.5000},  # Top-right corner
    'southwest': {'lat': 23.7000, 'lng': 90.3500}   # Bottom-left corner
}

# Helper function to check if a location is within the defined bounds
def is_within_bounds(lat, lng):
    return (dhaka_bounds['southwest']['lat'] <= lat <= dhaka_bounds['northeast']['lat']) and \
           (dhaka_bounds['southwest']['lng'] <= lng <= dhaka_bounds['northeast']['lng'])

# Loop through each origin, destination, and timestamp pair
for origin, destination, timestamp in zip(origins, destinations, timestamps):
    try:
        # Combine the time with today's date (or set a specific date if required)
        today = datetime.now().date()  # Use today's date
        timestamp = datetime.combine(today, timestamp.time())  # Combine date with time
        
        # Attempt to geocode both origin and destination
        origin_geocode = gmaps.geocode(origin)
        destination_geocode = gmaps.geocode(destination)

        # Validate geocode results
        if not origin_geocode or not destination_geocode:
            print(f"Geocode failed for {origin} or {destination}.")
            # Attempt to adjust locations by appending "near Dhaka"
            origin = f"{origin}, near Dhaka, Bangladesh"
            destination = f"{destination}, near Dhaka, Bangladesh"
            origin_geocode = gmaps.geocode(origin)
            destination_geocode = gmaps.geocode(destination)
            if not origin_geocode or not destination_geocode:
                print(f"Retry failed for adjusted {origin} or {destination}. Skipping.")
                continue

        # Get lat/lng for origin and destination
        origin_latlng = origin_geocode[0]['geometry']['location']
        destination_latlng = destination_geocode[0]['geometry']['location']

        # If outside bounds, add "Dhaka" to help Google interpret it as a near-Dhaka query
        if not is_within_bounds(origin_latlng['lat'], origin_latlng['lng']):
            origin = f"{origin}, Dhaka, Bangladesh"
        if not is_within_bounds(destination_latlng['lat'], destination_latlng['lng']):
            destination = f"{destination}, Dhaka, Bangladesh"

        # Re-geocode if bounds adjustment is made
        origin_geocode = gmaps.geocode(origin)
        destination_geocode = gmaps.geocode(destination)
        if not origin_geocode or not destination_geocode:
            print(f"Failed to adjust bounds for {origin} or {destination}. Skipping.")
            continue

        origin_latlng = origin_geocode[0]['geometry']['location']
        destination_latlng = destination_geocode[0]['geometry']['location']

    except Exception as e:
        print(f"Error geocoding {origin} or {destination}: {e}")
        continue

    # Prepare result dictionary
    result = {
        'Origin': origin,
        'Destination': destination,
        'Timestamp': timestamp,
        'Car_Distance': None,
        'Car_Duration': None,
        'Bus_Distance': None,
        'Bus_Duration': None,
        'Walking_Distance': None,
        'Walking_Duration': None,
        'Bike_Distance': None,
        'Bike_Duration': None
    }

    # Define travel modes and their respective Google Maps API mode names
    travel_modes = {
        'Car': 'driving',
        'Bus': 'transit',
        'Walking': 'walking',
        'Bike': 'driving'  # Treat 'Bike' as 'driving' for motorbike scenarios
    }

    # Fetch data for each mode
    for mode_name, mode_value in travel_modes.items():
        try:
            # Convert timestamp to Unix time format (seconds since epoch)
            departure_time = int(timestamp.timestamp())

            if mode_name == 'Bus':
                response = gmaps.distance_matrix(
                    origins=[origin_latlng],
                    destinations=[destination_latlng],
                    mode=mode_value,
                    region="bd",
                    transit_mode=['bus'],
                    departure_time=departure_time  # Specify the time of the trip
                )
            else:
                response = gmaps.distance_matrix(
                    origins=[origin_latlng],
                    destinations=[destination_latlng],
                    mode=mode_value,
                    region="bd",
                    departure_time=departure_time  # Specify the time of the trip
                )

            if response['status'] != 'OK':
                print(f"API response status error for {origin} to {destination}: {response['status']}")
                continue

            element = response['rows'][0]['elements'][0]

            if element['status'] != 'OK':
                print(f"Element status error for {origin} to {destination} using {mode_name}: {element['status']}")
                continue

            distance = element.get('distance', {}).get('text', None)
            duration = element.get('duration', {}).get('text', None)

            result[f'{mode_name}_Distance'] = distance
            result[f'{mode_name}_Duration'] = duration

        except Exception as e:
            print(f"Error fetching data for {origin} to {destination} using {mode_name}: {e}")
            time.sleep(1)  # Pause to avoid hitting rate limits

    # Append the result to the results list
    results.append(result)

# Convert results into a DataFrame
results_df = pd.DataFrame(results)

# Save the results to an Excel file
output_file = r'G:\4-2\CE-400\Google MAP\results_location_egress_Time.xlsx'  # Specify the path where you want to save the results
results_df.to_excel(output_file, index=False)

print(f'Results saved to {output_file}')

