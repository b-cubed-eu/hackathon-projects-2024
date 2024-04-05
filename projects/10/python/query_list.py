import csv

def create_string_from_csv(file_path):
    # Initialize an empty list to store values
    values = []

    # Read values from the CSV file
    with open(file_path, 'r') as csvfile:
        csv_reader = csv.reader(csvfile)
        for row in csv_reader:
            # Assuming each row contains a single value
            values.extend(row)

    # Create a string with the values
    string_piece1 = "SELECT"
    string_piece2 = "AS value \n UNION ALL"
    result_string = "{} {} {}".format(string_piece1, ", ".join(map(str, values)), string_piece2)

    return result_string

# Example usage
file_path = 'citizen_science_key.csv'
result_string = create_string_from_csv(file_path)
print("Resulting string:", result_string)
