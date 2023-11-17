#! /usr/bin/python3

from bs4 import BeautifulSoup
import lxml
import json
import sys

def parse_xml(fp):
    sys.stderr.write("Parsing xml\n")
    soup = BeautifulSoup(fp, "xml")
    feeds = []
    for tag in soup.find_all("outline"):
        if not tag.has_attr("xmlUrl"):
            continue
        else:
            feeds.append(dict({"feedUrl": tag["xmlUrl"], "type": tag["type"], "text": tag["text"]}))

    return feeds

if __name__ == "__main__":
    sys.stderr.write("Running the python file.\n")
    feeds = parse_xml(sys.stdin)
    enc = json.JSONEncoder()
    sys.stdout.write(enc.encode(feeds))
    sys.exit(0)
