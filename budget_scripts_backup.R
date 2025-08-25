############## works except tags 
import requests
import pandas as pd
from datetime import datetime, timezone, time, timedelta
from google.cloud import bigquery

# -------------------------------
# Configuration
# -------------------------------
API_KEY = "YWRkMDcwNmMtNjYyNi00MDlkLWE4YzgtNzg1Zjg3MWIwMzAw"
WORKSPACE_ID = "61cdf900e3d999473731bb1c"

BQ_PROJECT = "slstrategy"
BQ_DATASET = "Budget_System"
BQ_TABLE = "Clockify_Detail_Report"

headers = {
  "X-Api-Key": API_KEY,
  "Content-Type": "application/json"
  # If required, include: "x-addon-token": "<your-addon-token>"
}

# -------------------------------
# Helper: Format ISO dates with microseconds
# -------------------------------
def iso_date(dt):
  # Format using microseconds to match API expectation: YYYY-MM-DDTHH:MM:SS.ssssssZ
  formatted = dt.isoformat(timespec="microseconds").replace("+00:00", "Z")
print(f"[DEBUG] Formatted date: {formatted}")
return formatted

# -------------------------------
# Helper: Extract custom field value by name
# -------------------------------
def extract_custom_value(row, field_name):
  """Extracts the value for a custom field by name (case-insensitive) from the customFields list."""
custom = row.get("customFields", [])
if isinstance(custom, list):
  for field in custom:
  if field.get("name", "").strip().lower() == field_name.strip().lower():
  return field.get("value", "")
return ""

# -------------------------------
# "Get by ID" functions
# -------------------------------
def get_project_by_id(project_id):
  url = f"https://api.clockify.me/api/v1/workspaces/{WORKSPACE_ID}/projects/{project_id}"
params = {"hydrated": "true", "custom-field-entity-type": "TIMEENTRY"}
response = requests.get(url, headers=headers, params=params, timeout=10)
if response.status_code == 200:
  project = response.json()
return project.get("name", "Unknown")
else:
  print(f"[ERROR] Failed to get project {project_id}: {response.status_code} {response.text}")
return "Unknown"

def get_client_by_id(client_id):
  url = f"https://api.clockify.me/api/v1/workspaces/{WORKSPACE_ID}/clients/{client_id}"
response = requests.get(url, headers=headers, timeout=10)
if response.status_code == 200:
  client = response.json()
return client.get("name", "Unknown")
else:
  print(f"[ERROR] Failed to get client {client_id}: {response.status_code} {response.text}")
return "Unknown"

def get_task_by_id(task_id, project_id):
  url = f"https://api.clockify.me/api/v1/workspaces/{WORKSPACE_ID}/projects/{project_id}/tasks/{task_id}"
response = requests.get(url, headers=headers, timeout=10)
if response.status_code == 200:
  task = response.json()
return task.get("name", "Unknown")
else:
  print(f"[ERROR] Failed to get task {task_id} for project {project_id}: {response.status_code} {response.text}")
return "Unknown"

def get_tag_by_id(tag_id):
  url = f"https://api.clockify.me/api/v1/workspaces/{WORKSPACE_ID}/tags/{tag_id}"
response = requests.get(url, headers=headers, timeout=10)
if response.status_code == 200:
  tag = response.json()
return tag.get("name", "Unknown")
else:
  print(f"[ERROR] Failed to get tag {tag_id}: {response.status_code} {response.text}")
return "Unknown"

def get_users():
  base_url = f"https://api.clockify.me/api/v1/workspaces/{WORKSPACE_ID}/users"
params = {
  "page": "1",
  "page-size": "5000",
  "sort-column": "EMAIL",
  "sort-order": "ASCENDING",
  "include-roles": "false"
}
print(f"[DEBUG] GET Users from {base_url} with params {params}")
response = requests.get(base_url, headers=headers, params=params, timeout=10)
if response.status_code == 200:
  users = {}
for user in response.json():
  uid = str(user['id'])
users[uid] = {"name": user.get("name", ""), "email": user.get("email", "")}
print(f"[DEBUG] Fetched {len(users)} users.")
return users
else:
  print(f"[ERROR] Failed to fetch users: {response.status_code} {response.text}")
return {}

# -------------------------------
# Detailed Report Fetch with Logging
# -------------------------------
def get_detailed_report():
  # Fixed start date: January 1, 2025 at 00:00:00 UTC
  start_date = datetime(2025, 1, 1, 0, 0, 0, 0, tzinfo=timezone.utc)
# Dynamic end date: current UTC time
end_date = datetime.now(timezone.utc)
date_range_start = iso_date(start_date)
date_range_end = iso_date(end_date)

url = f"https://reports.api.clockify.me/v1/workspaces/{WORKSPACE_ID}/reports/detailed"
print(f"[DEBUG] GET Detailed Report from {url}")
all_entries = []
page = 1

payload = {
  "amountShown": "COST",
  "amounts": ["COST"],
  "dateFormat": "YYYY-MM-DD",
  "dateRangeStart": date_range_start,
  "dateRangeEnd": date_range_end,
  "dateRangeType": "ABSOLUTE",
  "description": "",
  "approvalState": "ALL",
  "detailedFilter": {
    "options": {"totals": "EXCLUDE"},
    "page": 1,
    "pageSize": 500,
    "sortColumn": "DATE"
  },
  "exportType": "JSON_V1",  # Use JSON_V1 to pull additional fields if available
  "rounding": False,
  "sortOrder": "ASCENDING",
  "userLocale": "en"
}

print("[DEBUG] Detailed Report Payload:")
print(payload)

while True:
  payload["detailedFilter"]["page"] = page
print(f"[DEBUG] Fetching page {page} ...")
response = requests.post(url, headers=headers, json=payload, timeout=20)
print(f"[DEBUG] Response status for page {page}: {response.status_code}")
if response.status_code == 200:
  time_entries = response.json().get("timeentries", [])
print(f"[DEBUG] Number of entries returned on page {page}: {len(time_entries)}")
if not time_entries:
  break
all_entries.extend(time_entries)
page += 1
else:
  print(f"[ERROR] Error fetching report: {response.status_code}, {response.text}")
break

print(f"[DEBUG] Total fetched entries: {len(all_entries)}")
return all_entries

# -------------------------------
# Transform Data with Lookup Dictionaries
# -------------------------------
def transform_data(data, users_dict, projects_dict, clients_dict, tasks_dict, tags_dict):
  if not data:
  print("[DEBUG] No data found, skipping transformation.")
return None

print("[DEBUG] Starting transformation of data.")
df = pd.DataFrame(data)
print("[DEBUG] DataFrame created. Columns:", df.columns.tolist())

# Split timeInterval into date/time fields
df["Start Date"] = df["timeInterval"].apply(lambda x: x.get("start", "")[:10])
df["Start Time"] = df["timeInterval"].apply(lambda x: x.get("start", "")[11:19])
df["End Date"] = df["timeInterval"].apply(lambda x: x.get("end", "")[:10])
df["End Time"] = df["timeInterval"].apply(lambda x: x.get("end", "")[11:19])
df["Duration (decimal)"] = df["timeInterval"].apply(lambda x: float(x.get("duration", 0)) / 3600 if x.get("duration") else 0)
df["Duration (h)"] = df["Duration (decimal)"].apply(lambda x: round(x, 2))
print("[DEBUG] Time fields extracted.")

# Use lookup dictionaries built from "get by id" functions:
df["Project"] = df["projectId"].apply(lambda x: projects_dict.get(str(x), "Unknown"))
df["Client"] = df["clientId"].apply(lambda x: clients_dict.get(str(x), "Unknown"))
# For tasks, use tuple (taskId, projectId)
df["Task"] = df.apply(lambda row: tasks_dict.get((str(row["taskId"]), str(row["projectId"])), "Unknown")
                      if pd.notna(row["taskId"]) else "", axis=1)
df["User"] = df["userId"].apply(lambda x: users_dict.get(str(x), {}).get("name", "Unknown"))
df["Email"] = df["userId"].apply(lambda x: users_dict.get(str(x), {}).get("email", ""))
df["Description"] = df["description"]

# Process Tags. Check for both "tagIds" and "tags".
if "tagIds" in df.columns:
  print("[DEBUG] 'tagIds' column found in data.")
df["Tags"] = df["tagIds"].apply(
  lambda tlist: ", ".join([tags_dict.get(str(t), "Unknown") for t in tlist]) if isinstance(tlist, list) and tlist else ""
)
elif "tags" in df.columns:
  print("[DEBUG] 'tags' column found in data.")
def process_tag(t):
  if isinstance(t, str):
  return t
elif isinstance(t, list):
  names = []
for x in t:
  if isinstance(x, dict):
  names.append(x.get("name", ""))
else:
  names.append(str(x))
return ", ".join(names)
elif isinstance(t, dict):
  return t.get("name", "")
else:
  return ""
df["Tags"] = df["tags"].apply(process_tag)
else:
  print("[DEBUG] No tag field found in data.")
df["Tags"] = ""

if not df.empty:
  print("[DEBUG] Sample row tag data:", df.iloc[0]["Tags"])

print("[DEBUG] Dynamic fields mapped.")

# Map remaining fields
df["Type"] = df.get("type", "")
df["Billable"] = df["billable"]
df["Cost Rate (USD)"] = df["costRate"]
df["Cost Amount (USD)"] = df["costAmount"]

# Extract Approval and ApprovedBy from customFields (using exact names as they appear in the UI)
df["Approval"] = df.apply(lambda row: extract_custom_value(row, "Approval"), axis=1)
df["ApprovedBy"] = df.apply(lambda row: extract_custom_value(row, "Approved by"), axis=1)

final_cols = [
  "Project", "Client", "Description", "Task", "User", "Email", "Tags",
  "Type", "Billable", "Start Date", "Start Time", "End Date", "End Time",
  "Duration (h)", "Duration (decimal)", "Cost Rate (USD)", "Cost Amount (USD)",
  "Approval", "ApprovedBy"
]
df = df[final_cols]

# Rename columns for BigQuery compliance.
rename_mapping = {
  "Start Date": "Start_Date",
  "Start Time": "Start_Time",
  "End Date": "End_Date",
  "End Time": "End_Time",
  "Duration (h)": "Duration_h",
  "Duration (decimal)": "Duration_decimal",
  "Cost Rate (USD)": "Cost_Rate_USD",
  "Cost Amount (USD)": "Cost_Amount_USD"
}
df = df.rename(columns=rename_mapping)
print("[DEBUG] Final DataFrame columns:", df.columns.tolist())
return df

# -------------------------------
# Upload Data to BigQuery with Logging
# -------------------------------
def upload_to_bigquery(df):
  if df is None or df.empty:
  print("[DEBUG] No data to upload to BigQuery.")
return
print("[DEBUG] Uploading data to BigQuery.")
client = bigquery.Client()
table_id = f"{BQ_PROJECT}.{BQ_DATASET}.{BQ_TABLE}"
print(f"[DEBUG] BigQuery table: {table_id}")
job_config = bigquery.LoadJobConfig(write_disposition=bigquery.WriteDisposition.WRITE_TRUNCATE)
job = client.load_table_from_dataframe(df, table_id, job_config=job_config)
job.result()
print(f"[DEBUG] Data successfully uploaded to {table_id}.")

# -------------------------------
# Build Lookup Dictionaries via "Get by ID" Endpoints
# -------------------------------
def build_lookup_dictionaries(data):
  # Extract unique IDs from the detailed report
  project_ids = set()
client_ids = set()
task_keys = set()  # tuples of (taskId, projectId)
tag_ids = set()

for entry in data:
  if entry.get("projectId"):
  project_ids.add(str(entry["projectId"]))
if entry.get("clientId"):
  client_ids.add(str(entry["clientId"]))
if entry.get("taskId"):
  task_keys.add((str(entry["taskId"]), str(entry["projectId"])))
# Check for tagIds field or tags (if dict)
if entry.get("tagIds") and isinstance(entry["tagIds"], list):
  for t in entry["tagIds"]:
  tag_ids.add(str(t))
elif entry.get("tags") and isinstance(entry["tags"], dict):
  tag_id = entry["tags"].get("_id")
if tag_id:
  tag_ids.add(str(tag_id))

print(f"[DEBUG] Unique project IDs: {project_ids}")
print(f"[DEBUG] Unique client IDs: {client_ids}")
print(f"[DEBUG] Unique task keys (taskId, projectId): {task_keys}")
print(f"[DEBUG] Unique tag IDs: {tag_ids}")

projects_dict = {pid: get_project_by_id(pid) for pid in project_ids}
clients_dict = {cid: get_client_by_id(cid) for cid in client_ids}
tasks_dict = {(tid, pid): get_task_by_id(tid, pid) for (tid, pid) in task_keys}
tags_dict = {tid: get_tag_by_id(tid) for tid in tag_ids}

return projects_dict, clients_dict, tasks_dict, tags_dict

# -------------------------------
# Run Pipeline with Logging
# -------------------------------
def run_pipeline():
  print("[INFO] Starting pipeline.")
users_dict = get_users()
data = get_detailed_report()
print("[INFO] Detailed report fetched.")

projects_dict, clients_dict, tasks_dict, tags_dict = build_lookup_dictionaries(data)

df = transform_data(data, users_dict, projects_dict, clients_dict, tasks_dict, tags_dict)
if df is not None:
  print("[INFO] Transformation complete. DataFrame shape:", df.shape)
print("[INFO] Sample transformed row:")
print(df.head(1))
upload_to_bigquery(df)
print("[INFO] Pipeline complete.")

if __name__ == "__main__":
  run_pipeline()