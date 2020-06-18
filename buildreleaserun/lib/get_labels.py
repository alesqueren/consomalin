import json, fileinput

for line in fileinput.input():
    data = json.loads(line)
    for pipeline in data["pipelines"]:
	try:
            if pipeline["stages"][-2]["result"] == "Passed":
	        print(pipeline["label"])
        except:
            pass
