#! /usr/bin/python3

import feedparser
import json
import fileinput
import sys

def episode_info(entry):
    def get_episode_url():
        for link in entry.links:
            if link.rel == "enclosure":
                return link.href
            else:
                continue
    out = dict()
    out["title"] = entry.get("title", None)
    out["description"] = entry.get("description", None)
    out["date_published"] = entry.get("published", None)
    out["file_url"] = get_episode_url()
    out["duration"] = entry.get("itunes_duration", None)
    if entry.has_key("tags"):
        out["tags"] = [t["term"] for t in entry.tags]
    else:
        out["tags"] = []
    return out

def episodes_info(entries):
    out = []
    for entry in entries:
        out.append(episode_info(entry))
    return out

def podcast_info(feed):
    out = dict()
    out["title"] = feed.title
    out["description"] = feed.description
    out["cover_art_url"] = feed.image.href
    if feed.has_key("tags"):
        out["tags"] = [t["term"] for t in feed.tags]
    else:
        out["tags"] = []
    return out

if __name__ == "__main__":
#    feedUrl = sys.argv[1]
    feed_str = sys.stdin.read()
    d = feedparser.parse(feed_str)
    #sys.stdin = open("/dev/tty")
    out = podcast_info(d.feed)
    out["episodes"] = episodes_info(d.entries)
    enc = json.JSONEncoder()
    sys.stdout.write(enc.encode(out))
    sys.exit(0)
