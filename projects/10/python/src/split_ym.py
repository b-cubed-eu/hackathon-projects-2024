import csv

def read_values_from_csv(file_path):
    values = []
    with open(file_path, 'r') as csvfile:
        csv_reader = csv.reader(csvfile)
        for row in csv_reader:
            values.extend(row)
    return values

def add_column_to_csv(input_file, output_file):

    # Open the input and output CSV files
    with open(input_file, 'r', newline='') as infile, \
            open(output_file, 'w', newline='') as outfile:
        reader = csv.reader(infile, delimiter='\t')
        writer = csv.writer(outfile)

        # Process each row
        for row in reader:
            # Check if the value in the specified column is in the list
            split_row = row[0].split('-')
            if len(split_row) > 1:
                row.append(split_row[0])
                row.append(split_row[1])
            # Write the updated row to the output file
            writer.writerow(row)

# Example usage
input_file = './output/output2.csv'
output_file = './output/output2_ym_comma.csv'

add_column_to_csv(input_file, output_file)
print("Columns added successfully.")
