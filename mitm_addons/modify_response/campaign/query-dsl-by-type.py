import json
import pathlib
from mitmproxy import ctx  # type: ignore


__dir = pathlib.Path(__file__).parent


def get_json() -> str:
    ctx.log.warn(__dir / "query-dsl-by-type-dsl.json")
    dsl = json.dumps(load_dsl())
    res = {"msg": "success", "code": 0, "data": {"dslContent": dsl, "metricsCid": ""}}

    return json.dumps(res)


def load_dsl():
    with open(__dir / "query-dsl-by-type-dsl.json") as f:
        return json.load(f)
