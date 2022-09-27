"""
Basic skeleton of a mitmproxy addon.

Run as follows: mitmproxy -s anatomy.py
"""
from mitmproxy import ctx

API_PATH = "1960-stavs-sl-canting.sjst.test.meituan.com/diancan/menu/api"


class Counter:
    def __init__(self):
        self.num = 0

    def request(self, flow):
        request = flow.request
        host, path = (
            request.host,
            request.path,
        )
        with_out_schema = f"{host}{path}"
        ctx.log.info(f"request with_out_schema: {with_out_schema}")
        if with_out_schema == API_PATH:
            ctx.log.info("123")

    # def response(self, flow):
    # ctx.log.info(f"response content: {flow.response.content}")


addons = [Counter()]
