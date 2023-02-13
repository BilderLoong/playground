# Testing command
# curl https://example.com -x http://localhost:8080

from mitmproxy import ctx
import pathlib
import json



# The part of the  api path aim to replace.
MOCKED_API_PATH='orderDetail'

# Path should relative to the script file.
MOCKED_JSON_FILE_NAME = f'{MOCKED_API_PATH}.json'


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

    if MOCKED_API_PATH in request.url:
        # Read mocked file.
       with open(pathlib.Path(__file__).parent / MOCKED_JSON_FILE_NAME, 'r') as f:
            data = f.read() 
            try:
                json.loads(data)
            except ValueError:
               ctx.log.error("Mocked File doesn't contain valid JSON:", flow.response.get_text())

       # https://docs.mitmproxy.org/stable/api/mitmproxy/http.html#Message.set_text
       response.set_text(data) 

        # get_text(): https://docs.mitmproxy.org/stable/api/mitmproxy/http.html#Message.get_text
       ctx.log.info(flow.response.get_text())
     
