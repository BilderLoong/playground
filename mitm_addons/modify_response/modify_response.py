# Testing command
# curl https://example.com -x http://localhost:8080

import json
import pathlib

from typing import Literal, Callable, Dict, Any
import functools


from mitmproxy import ctx
import logging

# https://docs.python.org/ja/3/howto/logging.html
logging.basicConfig(encoding="utf-8", level=logging.DEBUG)

__dir = pathlib.Path(__file__).parent

# LOG_LEVEL =

# https://docs.python.org/3/library/typing.html
# https://fastapi.tiangolo.com/python-types/?h=type#motivation
MOCKED_API_PATH_TYPE = (
    Literal["queryMemberCoupons"]
    | Literal["search"]
    | Literal["pageShopList"]
    | Literal["provinceCityList"]
    | Literal["orderList"]
    | Literal["loadFirstPage"]
    | Literal["loadFMPInfo"]
    | Literal["pageSpuInfo"]
    | Literal["queryNeedSignAgreements"]
    | Literal["provinceCityList"]
    | Literal["orderConfirm"]
    | Literal["orderDetail"]
    | Literal["queryDishMoreInfo"]
    | Literal["order-detail"]
    | Literal["order-list"]
    | Literal["payOrder"]
    | Literal["couponUseInfo"]
    | Literal["postOrder"]
    | Literal["login"]
    | Literal["list"]
    | Literal["load-fmp-info"]
    | Literal["queryGrouponCouponInfo"]
    | Literal["query-groupon-coupon-info"]
)

# The part of the  api path aim to replace.
MODIFIED_API_PATHS: list[MOCKED_API_PATH_TYPE] = [
    "queryGrouponCouponInfo",
    "query-groupon-coupon-info",
]

# def request(flow):
#     ctx.log.info(flow.request.get_text())
#     flow.request.headers["myheader"] = "value"

# pretty_host takes the "Host" header of the request into account,
# which is useful in transparent mode where we usually only have the IP
# otherwise.
# if flow.request.pretty_host == "google.com":
#     flow.request.host =  "baidu.com"

"""
@param flow: https://docs.mitmproxy.org/stable/api/mitmproxy/http.html#HTTPFlow
"""


def response(flow):
    logging.debug(f"flow.request.url: {flow.request.url}")
    if not any([i in flow.request.url for i in MODIFIED_API_PATHS]):
        return

    replace_response_body(flow)

    # modify_response_body(flow, dropUnwantedKeys)


def replace_response_body(flow):
    # https://docs.mitmproxy.org/stable/api/mitmproxy/http.html#Request
    response = flow.response
    # https://docs.mitmproxy.org/stable/api/mitmproxy/http.html#Response
    request = flow.request
    for path in MODIFIED_API_PATHS:
        if path in request.url:
            # Path should relative to the script file.
            MOCKED_JSON_FILE_NAME = f"{path}.json"
            json_str = None
            try:
                # Read mocked file.
                with open(__dir / MOCKED_JSON_FILE_NAME, "r") as f:
                    json_str = f.read()

                    try:
                        json.loads(json_str)
                    except ValueError:
                        ctx.log.error(
                            "Mocked File doesn't contain valid JSON:",
                            flow.response.get_text(),
                        )
            except FileNotFoundError:
                ctx.log.info(f"File {MOCKED_JSON_FILE_NAME} not found.")

            else:
                # https://docs.mitmproxy.org/stable/api/mitmproxy/http.html#Message.set_text
                response.set_text(json_str)


def dropUnwantedKeys(fmp: Dict[str, Any]):
    try:
        data: Dict[str, Any] = fmp.get("data")
        data.get("moduleData").pop("groupon-coupon-swiper")
        data.get("shopInfo").pop("groupCouponInfo")
    except KeyError:
        pass

    return fmp


def modify_response_body(flow, cb: Callable[[Dict[str, Any]], Dict[str, Any]]):
    response = flow.response
    try:
        response_dict = json.loads(response.get_text())

        modified = cb(response_dict)
        # logging.info(f"modified: {modified}")

        response.set_text(json.dumps(modified))

    except ValueError:
        logging.info(
            f"Response body is not valid JSON request path: {flow.request.pretty_url}"
        )


def add_adjacent_key(data, target_key, added_key, added_value):
    if isinstance(data, dict):
        updated_data = data.copy()  # Create a shallow copy of the original dictionary

        if target_key in updated_data:
            updated_data[added_key] = added_value

        for key, value in updated_data.items():
            # Recursively update nested dictionaries
            updated_data[key] = add_adjacent_key(
                value, target_key, added_key, added_value
            )

        return updated_data

    elif isinstance(data, list):
        updated_data = []
        for item in data:
            updated_data.append(
                add_adjacent_key(item, target_key, added_key, added_value)
            )

        return updated_data

    return data  # Return non-dictionary values as is


def compose(*functions):
    def compose2(f, g):
        return lambda x: f(g(x))

    return functools.reduce(compose2, functions, lambda x: x)
