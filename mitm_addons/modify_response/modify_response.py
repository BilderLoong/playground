# Testing command
# curl https://example.com -x http://localhost:8080

import json
import pathlib
from typing import Literal

from mitmproxy import ctx

# https://docs.python.org/3/library/typing.html
# https://fastapi.tiangolo.com/python-types/?h=type#motivation
MOCKED_API_PATH_TYPE = Literal['queryMemberCoupons'] | Literal['search'] | Literal['pageShopList'] | Literal['provinceCityList'] | Literal['orderList'] | Literal['loadFirstPage'] | Literal[
    'loadFMPInfo'] | Literal['pageSpuInfo'] | Literal['queryNeedSignAgreements'] | Literal['provinceCityList'] | Literal['orderConfirm'] | Literal['orderDetail'] | Literal['queryDishMoreInfo']

# The part of the  api path aim to replace.

MOCKED_API_PATHS: list[MOCKED_API_PATH_TYPE] = ['orderConfirm']


# def request(flow):
#     ctx.log.info(flow.request.get_text())
# flow.request.headers["myheader"] = "value"

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

    for PATH in MOCKED_API_PATHS:
        ctx.log.info(f'PATH: {PATH}')
        if PATH in request.url:
            # Path should relative to the script file.
            MOCKED_JSON_FILE_NAME = f'{PATH}.json'
            # Read mocked file.
            with open(pathlib.Path(__file__).parent / MOCKED_JSON_FILE_NAME, 'r') as f:
                json_str = f.read()

                try:
                    json.loads(json_str)
                except ValueError:
                    ctx.log.error(
                        "Mocked File doesn't contain valid JSON:", flow.response.get_text())

            # https://docs.mitmproxy.org/stable/api/mitmproxy/http.html#Message.set_text
            response.set_text(json_str)

            # get_text(): https://docs.mitmproxy.org/stable/api/mitmproxy/http.html#Message.get_text
            ctx.log.info(flow.response.get_text())
