# SQL-like Queries on CSV and Log Files

## Project Overview
This Haskell-based program allows users to perform SQL-like queries on CSV and structured `.log` files. It supports basic SQL operations like `SELECT` and saves the results to a text file.

## Features
- Load data from CSV and log files.
- Perform SQL-like queries on the loaded data.
- Save query results to a text file.
- Command-line interface for easy interaction.

## Requirements
- Haskell
- Stack

## Installation
1. Clone the repository:
    ```sh
    git clone https://github.com/ydelfinozen/sql-csv-query.git
    cd sql-csv-query
    ```

2. Build the project using Stack:
    ```sh
    stack build
    ```

## Usage
1. Run the program:
    ```sh
    stack exec sql-csv-query-exe
    ```

2. Available commands:
    - `load <file>`: Load a CSV file.
    - `query <SQL-like query>`: Execute a SQL-like query on the loaded data.
    - `save <file>`: Save the query results to a text file.
    - `help`: Display usage instructions.

## Example
1. Load a CSV file:
    ```
    > load example1.csv
    ```

2. Execute a query:
    ```
    > query SELECT Entity, Year FROM example1
    ```

3. Save the results:
    ```
    > save output.txt
    ```

## License
This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.

## Contact
For any questions or suggestions, please reach out to [ydelfinozen@gmail.com](mailto:ydelfinozen@gmail.com).

