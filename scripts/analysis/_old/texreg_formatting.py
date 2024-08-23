import os
import re

# Set the path to the folder containing the .tex files
folder_path = "/Users/gelkouh/Library/CloudStorage/OneDrive-Personal/Documents/School/UChicago/Thesis/MINING/coal-mining/output/tables/update"

# Loop over all .tex files in the folder
for filename in os.listdir(folder_path):
    if filename.endswith(".tex"):
        # Read the contents of the file
        with open(os.path.join(folder_path, filename), "r") as f:
            file_contents = f.read()

        # Use regular expressions to replace \begin{table} with \begin{table}[H]
        file_contents = re.sub(r"\\begin\{table\}", r"\\begin{table}[H]", file_contents)

        # Write the modified contents back to the file
        with open(os.path.join(folder_path, filename), "w") as f:
            f.write(file_contents)
