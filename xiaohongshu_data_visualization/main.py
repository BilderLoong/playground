import pandas as pd
import plotly.express as px
import pygwalker as pg


def main():
    # 1. Load your raw data (just a few examples here)

    # 2. Turn it into a Pandas DataFrame (a structured table)
    df = pd.read_json(
        "/Users/birudo/Projects/MediaCrawler/data/xhs/jsonl/search_contents_2026-05-11.jsonl",
        lines=True,
    )

    # Convert milliseconds to a real Date
    df["date"] = pd.to_datetime(df["time"], unit="ms")

    print(f"Rows before cleaning: {len(df)}")
    df = df.drop_duplicates(subset=["note_id"], keep="first")
    print(f"Rows after cleaning: {len(df)}")
    
    pg.walk(df)


if __name__ == "__main__":
    main()
