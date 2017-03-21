import csv
import urllib

BASE_URL_PATH = "https://wwws-cloud.lsd.ufcg.edu.br:42160/api/reservatorios/{lake_id}/monitoramento"

with open('../reservatorios_info.csv', 'rb') as csvfile:
	reader = csv.reader(csvfile)
	included_cols = [7]
	for row in reader:
		lake_ids = [row[i] for i in included_cols]
		for lake_id_str in lake_ids:
			try:
				lake_id = int(lake_id_str)
				download_url = BASE_URL_PATH.format(lake_id=lake_id)
				urllib.urlretrieve (download_url, '{lake_id}.json'.format(lake_id=lake_id))
			except :
				pass
