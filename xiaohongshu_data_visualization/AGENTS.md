# AGENTS.md

## Tool Chains

Python **≥3.13**, managed with **uv** (`pyproject.toml`, `uv.lock`).

## Data Schema — Xiaohongshu Posts JSONL

Source: `MediaCrawler/data/xhs/jsonl/search_contents_YYYY-MM-DD.jsonl` (one JSON object per line)

| Field | Type | Example | Notes |
|---|---|---|---|
| `note_id` | str | `65f98f0b0000000014005088` | Unique post ID |
| `type` | str | `normal` | Post type |
| `title` | str | `姗姗来迟的长新冠热搜` | |
| `desc` | str | `刚发现前几天热搜有过...` | Full text description |
| `video_url` | str | `""` | May be empty |
| `time` | int (ms) | `1710853899000` | Unix timestamp in **milliseconds** — parse with `pd.to_datetime(df["time"], unit="ms")` |
| `last_update_time` | int (ms) | `1710910258000` | |
| `user_id` | str | `592c5fd85e87e75e2969f37f` | |
| `nickname` | str | `呃` | |
| `avatar` | str | `https://sns-avatar-qc.xhscdn.com/...` | |
| `liked_count` | str | `1.3万` | **Chinese-formatted** — needs parsing: `万` = ×10,000 |
| `collected_count` | str | `3142` | Same format |
| `comment_count` | str | `3474` | Same format |
| `share_count` | str | `7896` | Same format |
| `ip_location` | str | `""` | May be empty |
| `image_list` | str | `http://sns-webpic-qc.xhscdn.com/...` | URL |
| `tag_list` | str | `新冠,脑雾,长新冠` | Comma-separated tags |
| `last_modify_ts` | int | `1778471285476` | |
| `note_url` | str | `https://www.xiaohongshu.com/explore/...` | |
| `source_keyword` | str | `长新冠` | Search keyword used to find this post |
| `xsec_token` | str | `AB5rA3DBeroBmU...` | |