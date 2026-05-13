import streamlit as st

import polars as pl
import plotly.express as px


def parse_count(col_name: str) -> pl.Expr:
    return (
        pl.when(pl.col(col_name).str.contains("万"))
        .then(
            pl.col(col_name)
            .str.replace("万", "")
            .cast(pl.Float64, strict=False)
            .fill_null(0)
            * 10000
        )
        .otherwise(pl.col(col_name).cast(pl.Int64(), strict=False).fill_null(0))
        .cast(pl.Int64())
        .alias(col_name)
    )


def clean_and_format_data(df: pl.DataFrame) -> pl.DataFrame:
    print(f"Rows before duplicating: {df.height}")
    df = df.unique(subset="note_id")
    print(f"Rows after duplicating: {df.height}")

    print(f"Rows before filtering: {df.height}")
    df = df.filter(
        pl.col("title").str.contains("长新冠").fill_null(False)
        | pl.col("desc").str.contains("长新冠").fill_null(False)
    )
    print(f"Rows after filtering: {df.height}")

    df = df.with_columns(
        pl.from_epoch("time", time_unit="ms").alias("date_time"),
        *[
            parse_count(col)
            for col in ["liked_count", "comment_count", "collected_count"]
        ],
    )

    return df


def process_data(df: pl.DataFrame, interval: str) -> pl.DataFrame:

    groupby_res = (
        df.sort(by="date_time")
        .with_columns(pl.col("date_time").dt.truncate(interval).alias("date"))
        .group_by("date")
        .agg(
            [
                pl.len().alias("post_count"),
                pl.col("liked_count").sum().alias("total_likes"),
                pl.col("collected_count").sum().alias("total_collected"),
                pl.col("comment_count").sum().alias("total_comments"),
            ],
        )
    )

    return groupby_res.sort(by="date")


def main():
    df = pl.read_ndjson(
        "/Users/birudo/Projects/MediaCrawler/data/xhs/jsonl/search_contents_2026-05-11.jsonl",
    )

    df = clean_and_format_data(df)
    # 1. Add a UI control for semantic aggregation
    granularity_map = {
        "Daily": "1d",
        "Weekly": "1w",
        "Monthly": "1mo",
        "Quarterly": "3mo",
        "Yearly": "1y",
    }

    selected_view = st.select_slider(
        "Select Time Granularity", options=list(granularity_map.keys())
    )
    res = process_data(df, interval=granularity_map[selected_view])

    fig = px.line(
        res.to_pandas(),
        x="date",
        y=["post_count", "total_likes", "total_collected", "total_comments"],
    ).update_xaxes(rangeslider_visible=True)

    # Render natively in Streamlit
    st.plotly_chart(fig, use_container_width=True)

    st.dataframe(res)

    # Plotly Express loves Pandas, so convert your Polars df simply:


if __name__ == "__main__":
    main()
