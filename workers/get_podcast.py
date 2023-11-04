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
    out["title"] = entry.title
    out["description"] = entry.description
    out["date_published"] = entry.published
    out["file_url"] = get_episode_url()
    out["duration"] = entry.itunes_duration
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
    enc = json.JSONEncoder()
    feedUrl = sys.argv[1]
    d = feedparser.parse(feedUrl)
    out = podcast_info(d.feed)
    out["episodes"] = episodes_info(d.entries)
    print(enc.encode(out))
    sys.exit(0)
