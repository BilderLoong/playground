import json
import string
import requests


ANKI_CONNECT_URL = "http://127.0.0.1:8765"

# proxies = {"http": "http://localhost:8080"}


def _get_request_dict(action_name: str, **params):
    res = {"action": action_name, "version": 6, "params": params}
    return res


def _invoke(action_name: str, **params):
    r = requests.post(
        ANKI_CONNECT_URL,
        data=json.dumps(_get_request_dict(action_name, **params)),
        # proxies=proxies,
    )
    res = r.json()

    # If response contain error, raise exception.
    if res["error"] is not None:
        raise Exception(res["error"])

    return res["result"]


def _generate_word_query(word: str):
    return f"expression:{word} note:odh_默认模板 -is:suspended"


def is_know_word(word: str):
    cards_id = _invoke("findCards", query=_generate_word_query(word))
    return bool(len(cards_id))
