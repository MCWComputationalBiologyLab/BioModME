import os
import glob

def count_lines_in_files(directory, file_extensions):
    total_lines = 0
    file_counts = {}

    for ext in file_extensions:
        file_counts[ext] = {"count": 0, "lines": 0}

    for ext in file_extensions:
        for filepath in glob.iglob(os.path.join(directory, '**', f'*{ext}'), recursive=True):
            if os.path.isfile(filepath):
                try:
                    with open(filepath, 'r', encoding='utf-8') as file:
                        line_count = sum(1 for line in file)
                        total_lines += line_count
                        file_counts[ext]["count"] += 1
                        file_counts[ext]["lines"] += line_count
                        print(f"{filepath}: {line_count} lines")
                except Exception as e:
                    print(f"Error processing {filepath}: {e}")

    print("\nSummary:")
    for ext, counts in file_counts.items():
        print(f"Total {ext} files analyzed: {counts['count']}")
        print(f"Total lines in {ext} files: {counts['lines']}")
    print(f"\nTotal files analyzed: {sum(counts['count'] for counts in file_counts.values())}")
    print(f"Total lines in all files: {total_lines}")

# Change the directory to the directory you want to search in
directory_to_search = "."  # Current directory

# Define the file extensions to search for
file_extensions = [".js", ".css"]

count_lines_in_files(directory_to_search, file_extensions)
