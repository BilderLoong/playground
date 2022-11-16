from mitmproxy import ctx

# Testing command
# curl https://example.com -x http:localhost:8080

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

    if "orderConfirm" in request.url:
        # https://docs.mitmproxy.org/stable/api/mitmproxy/http.html#Message.set_text
        # response.set_text('123') 
        # get_text(): https://docs.mitmproxy.org/stable/api/mitmproxy/http.html#Message.get_text
        ctx.log.info(flow.response.get_text())
     
