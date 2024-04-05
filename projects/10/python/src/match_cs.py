import csv

def read_values_from_csv(file_path):
    values = []
    with open(file_path, 'r') as csvfile:
        csv_reader = csv.reader(csvfile)
        for row in csv_reader:
            values.extend(row)
    return values

def add_column_to_csv(input_file, output_file, value_list_file):
    # Read values from the CSV file
    value_list = read_values_from_csv(value_list_file)

    # Open the input and output CSV files
    with open(input_file, 'r', newline='') as infile, \
            open(output_file, 'w', newline='') as outfile:
        reader = csv.reader(infile, delimiter='\t')
        writer = csv.writer(outfile, delimiter='\t')

        # Process each row
        for row in reader:
            # Check if the value in the specified column is in the list
            print(row)
            if row[3] in value_list:
                # If it is, append 1 to the row
                row.append('1')
            else:
                # If not, append 0
                row.append('0')

            # Write the updated row to the output file
            writer.writerow(row)

# Example usage
input_file = './data/cube_maarten/input.csv'
output_file = './output/output2.csv'
value_list_file = './data/citizen_science_key.csv'  # CSV file containing the list of values

add_column_to_csv(input_file, output_file, value_list_file)
print("Column added successfully.")
