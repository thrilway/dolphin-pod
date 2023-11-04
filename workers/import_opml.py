#! /usr/bin/python3

from bs4 import BeautifulSoup
import lxml
import json
import sys

def parse_xml(xml_string):
    soup = BeautifulSoup(xml_string, "xml")
    feeds = []
    for tag in soup.find_all("outline"):
        if not tag.has_attr("xmlUrl"):
            continue
        else:
            feeds.append(dict({"feedUrl": tag["xmlUrl"], "type": tag["type"], "text": tag["text"]}))

    return feeds

if __name__ == "__main__":
    in_s = open(sys.argv[1], 'r').read()
    enc = json.JSONEncoder()
    print(enc.encode(parse_xml(in_s)))
    sys.exit(0)
