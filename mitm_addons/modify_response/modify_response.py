# Testing command
# curl https://example.com -x http://localhost:8080

import json
import pathlib

from typing import Literal

from mitmproxy import ctx

__dir = pathlib.Path(__file__).parent

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
)

# The part of the  api path aim to replace.
MOCKED_API_PATHS: list[MOCKED_API_PATH_TYPE] = ["couponUseInfo"]


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
    # https://docs.mitmproxy.org/stable/api/mitmproxy/http.html#Request
    response = flow.response
    # https://docs.mitmproxy.org/stable/api/mitmproxy/http.html#Response
    request = flow.request

    for path in MOCKED_API_PATHS:
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

            # get_text(): https://docs.mitmproxy.org/stable/api/mitmproxy/http.html#Message.get_text
            # ctx.log.info(flow.response.get_text())


