
# Testing command
# curl https://example.com -x http://localhost:8080

import json
import pathlib
from typing import Literal

from mitmproxy import ctx

# https://docsest.host =  "baidu.com"

"""
@param flow: https://docs.mitmproxy.org/stable/api/mitmproxy/http.html#HTTPFlow
"""

# request event: https://docs.mitmproxy.org/stable/api/events.html#HTTPEvents.request
def request(flow):
    # https://docs.mitmproxy.org/stable/api/mitmproxy/http.html#Response
    request = flow.request
    if flow.request.pretty_host == 'example.com':
            flow.request.host = 'google.com'

def response(flow):
    # https://docs.mitmproxy.org/stable/api/mitmproxy/http.html#Request
    response = flow.response
