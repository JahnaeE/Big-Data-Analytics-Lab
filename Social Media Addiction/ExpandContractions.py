mport pandas as pd
import contractions

def expand_contractions(text):
    return contractions.fix(text)

#edit start and end to find error
def process_csv(input_file, output_file, start_row=115384, end_row=115472):
    df = pd.read_csv(input_file)
    df['Full Text'] = df['Full Text'].iloc[start_row-1:end_row].apply(expand_contractions)
    df.to_csv(output_file, index=False)

if __name__ == "__main__":
    input_csv_file = 'Reddit_clean.csv'
    output_csv_file = 'output.csv'
    start_row = 115384
    end_row = 115472

    process_csv(input_csv_file, output_csv_file, start_row, end_row)
