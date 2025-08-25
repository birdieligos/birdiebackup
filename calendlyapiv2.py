#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Jun 26 17:47:35 2025

@author: birdieligos
"""

import requests
import datetime
from google.cloud import bigquery

# Config
API_KEY = "eyJraWQiOiIxY2UxZTEzNjE3ZGNmNzY2YjNjZWJjY2Y4ZGM1YmFmYThhNjVlNjg0MDIzZjdjMzJiZTgzNDliMjM4MDEzNWI0IiwidHlwIjoiUEFUIiwiYWxnIjoiRVMyNTYifQ.eyJpc3MiOiJodHRwczovL2F1dGguY2FsZW5kbHkuY29tIiwiaWF0IjoxNzUwOTg0NDgwLCJqdGkiOiIxNzg2ZTY5ZC04OWYzLTQzYTctOGEzNS1iMDk2YjMyNThjOWYiLCJ1c2VyX3V1aWQiOiJFR0NHVDZKVlgzSDRQWlQ2In0.YlipqTJXX1UCMBwSBgiin31OFgVZE9WOj5pty1eyQG7MoB3ghg8PKHYWz_gAmUoEh572Arhs25C3Oo9Sl7G5kg"
BASE_URL = "https://api.calendly.com"
HEADERS = {
    "Authorization": f"Bearer {API_KEY}",
    "Content-Type": "application/json"
}
PROJECT_ID = "slstrategy"
DATASET_ID = "UBER_2025"
TABLE_ID = "uber_calendly_rawreport"

COLUMNS = [
    "User Name","Team","Invitee Name","Invitee First Name","Invitee Last Name",
    "Invitee Email","Invitee Time Zone","Invitee accepted marketing emails",
    "Text Reminder Number","Event Type Name","Start Date & Time","End Date & Time",
    "Location","Event Created Date & Time","Canceled","Canceled By",
    "Cancellation reason","Question 1","Response 1","Question 2","Response 2",
    "Question 3","Response 3","Question 4","Response 4","UTM Campaign",
    "UTM Source","UTM Medium","UTM Term","UTM Content","Salesforce UUID",
    "Event Price","Payment Currency","Guest Email(s)","Invitee Reconfirmed",
    "Marked as No-Show","Meeting Notes","Group","User Email","Event UUID",
    "Invitee UUID","Invitee scheduled by","Scheduling method"
]

def fetch_collection(url, params=None):
    items = []
    p = params.copy() if params else {}
    while True:
        resp = requests.get(url, headers=HEADERS, params=p)
        resp.raise_for_status()
        j = resp.json()
        items.extend(j.get("collection", []))
        token = j.get("pagination", {}).get("next_page_token")
        if not token:
            break
        p["page_token"] = token
    return items

def get_org_uri():
    resp = requests.get(f"{BASE_URL}/users/me", headers=HEADERS)
    resp.raise_for_status()
    r = resp.json().get("resource") or resp.json().get("data")
    return r.get("current_organization")

def flatten(e, inv, user_name, team_uri, inv_list):
    rec = {
        "User Name":                     user_name,
        "Team":                          team_uri,
        "Invitee Name":                  inv.get("name"),
        "Invitee First Name":            inv.get("first_name"),
        "Invitee Last Name":             inv.get("last_name"),
        "Invitee Email":                 inv.get("email"),
        "Invitee Time Zone":             inv.get("timezone"),
        "Invitee accepted marketing emails": inv.get("marketing_emails_opt_in"),
        "Text Reminder Number":          inv.get("text_reminder_number"),
        "Event Type Name":               e.get("event_type_name"),
        "Start Date & Time":             e.get("start_time"),
        "End Date & Time":               e.get("end_time"),
        "Location":                      e.get("location"),
        "Event Created Date & Time":     e.get("created_at"),
        "Canceled":                      e.get("status") == "canceled",
        "Canceled By":                   inv.get("canceled_by"),
        "Cancellation reason":           inv.get("cancellation_reason"),
    }
    qas = inv.get("questions_and_answers", [])
    for i in range(4):
        rec[f"Question {i+1}"] = qas[i]["question"] if len(qas) > i else None
        rec[f"Response {i+1}"] = qas[i]["answer"]   if len(qas) > i else None
    rec.update({
        "UTM Campaign":                  inv.get("utm_campaign"),
        "UTM Source":                    inv.get("utm_source"),
        "UTM Medium":                    inv.get("utm_medium"),
        "UTM Term":                      inv.get("utm_term"),
        "UTM Content":                   inv.get("utm_content"),
        "Salesforce UUID":               inv.get("tracking", {}).get("salesforce_uuid"),
        "Event Price":                   inv.get("payment", {}).get("amount"),
        "Payment Currency":              inv.get("payment", {}).get("currency"),
        "Guest Email(s)":                ",".join(i.get("email") for i in inv_list),
        "Invitee Reconfirmed":           inv.get("reconfirmed"),
        "Marked as No-Show":             inv.get("no_show"),
        "Meeting Notes":                 e.get("meeting_notes"),
        "Group":                         e.get("invitee_count", 0) > 1,
        "User Email":                    user_data.get("email"),
        "Event UUID":                    e.get("uri", "").split("/")[-1],
        "Invitee UUID":                  inv.get("uri", "").split("/")[-1],
        "Invitee scheduled by":          inv.get("created_by"),
        "Scheduling method":             inv.get("scheduling_method"),
    })
    return rec

def main():
    org_uri = get_org_uri()
    base = {"organization": org_uri, "count": 100}
    now = datetime.datetime.utcnow().replace(microsecond=0).isoformat() + "Z"

    past     = fetch_collection(f"{BASE_URL}/scheduled_events", {**base, "max_start_time": now})
    upcoming = fetch_collection(f"{BASE_URL}/scheduled_events", {**base, "min_start_time": now})
    events = past + upcoming

    invitees_map = {
        e.get("uri", "").split("/")[-1]: fetch_collection(
            f"{BASE_URL}/scheduled_events/{e.get('uri', '').split('/')[-1]}/invitees")
        for e in events
    }

    # Flatten all rows
    rows = []
    user_name = user_data.get("name")
    for e in events:
        uid = e.get("uri", "").split("/")[-1]
        for inv in invitees_map[uid]:
            rows.append(flatten(e, inv, user_name, org_uri, invitees_map[uid]))

    # BigQuery load with overwrite
    client = bigquery.Client(project=PROJECT_ID)
    table_ref = client.dataset(DATASET_ID).table(TABLE_ID)

    # Build schema
    schema = []
    for col in COLUMNS:
        if col in ("Invitee accepted marketing emails","Canceled","Invitee Reconfirmed","Marked as No-Show","Group"):
            ftype = "BOOL"
        elif col in ("Start Date & Time","End Date & Time","Event Created Date & Time"):
            ftype = "TIMESTAMP"
        elif col == "Event Price":
            ftype = "FLOAT"
        else:
            ftype = "STRING"
        schema.append(bigquery.SchemaField(col, ftype))

    job_config = bigquery.LoadJobConfig(
        schema=schema,
        write_disposition="WRITE_TRUNCATE"
    )
    job = client.load_table_from_json(rows, table_ref, job_config=job_config)
    job.result()

if __name__ == "__main__":
    user_data = requests.get(f"{BASE_URL}/users/me", headers=HEADERS).json().get("resource")
    main()