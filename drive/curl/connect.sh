user=goto.devnull@mailoo.org
pass=TTOjKLoXg1!

curl -v 'https://www.auchandrive.fr/drive/client/identification.formidentification' \
    -H 'User-Agent: Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/56.0.2924.87 Safari/537.36' \
    -b cookies.txt \
    --data 't%3Aformdata=XviXyDb4kGDnwAyJ6fsR0LFe0p4%3D%3AH4sIAAAAAAAAAFvzloG1XJVBOTknMzWvRN8zBUhmpmUmJ5Zk5udZpeYmZuaUJeZkpiSWpBYXMZjmF6XrJRYkJmek6pUkFqQWlxRVmuol5xel5mQm6SUlFqfqOSYBBROTS9wyU3NSVIJTS0oLVEMPcz8UPf6HiYHRh4E7OT%2BvpCg%2Fxy8xN7WEQcgnK7EsUT8nMS9dP7ikKDMv3bqioISBF2xxGNRi4t3nSKr7Aoryk1OLi4NLk3Izi4uBRh5el2KS9m3eOSYGhoqCcg0GNewWFyQWF5fnF6XA7S5kqGNgKGEQgEnA3U60ESATWMvlGGSwKy8GObEE6EcHvH5Mzs8tyM8D6izWA3uqBNOLM4M%2FSW7d0uLMxMDkw8ABsc0zBWQ9KHpSc1JzgQKg6AELgaKDA2J5vCGCaQAALR8OljkCAAA%3D&emailValidate='$user'&passwordValidate='$pass'&t%3Asubmit=%5B%22submit_1%22%2C%22submit_0%22%5D&t%3Azoneid=identification'

#curl 'https://www.auchandrive.fr/drive/client/identification.formidentification' \
#    -b cookies.txt \
#    -L \
#    -H 'Origin: https://www.auchandrive.fr' \
#    -H 'Accept-Encoding: gzip, deflate, br' \
#    -H 'Accept-Language: fr-FR,fr;q=0.8,en-US;q=0.6,en;q=0.4' \
#    -H 'User-Agent: Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/56.0.2924.87 Safari/537.36' \
#    -H 'Content-Type: application/x-www-form-urlencoded; charset=UTF-8' \
#    -H 'Accept: */*' \
#    -H 'Referer: https://www.auchandrive.fr/drive/client/identification' \
#    -H 'X-Requested-With: XMLHttpRequest' \
#    -H 'Connection: keep-alive' \
#    --data 't%3Aformdata=H4sIAAAAAAAAAFvzloG1XJVBOTknMzWvRN8zBUhmpmUmJ5Zk5udZpeYmZuaUJeZkpiSWpBYXMZjmF6XrJRYkJmek6pUkFqQWlxRVmuol5xel5mQm6SUlFqfqOSYBBROTS9wyU3NSVIJTS0oLVEMPcz8UPf6HiYHRh4E7OT%2BvpCg%2Fxy8xN7WEQcgnK7EsUT8nMS9dP7ikKDMv3bqioISBF2xxGNRi4t3nSKr7Aoryk1OLi4NLk3Izi4uBRh5el2KS9m3eOSYGhoqCcg0GNewWFyQWF5fnF6XA7S5kqGNgKGEQgEnA3U60ESATWMvlGGSwKy8GObEE6EcHvH5Mzs8tyM8D6izWA3uqBNOLM4M%2FSW7d0uLMxMDkw8ABsc0zBWQ9KHpSc1JzgQKg6AELgaKDA2J5vCGCaQAALR8OljkCAAA%3D&emailValidate=goto.devnull%40mailoo.org&passwordValidate=Auchan31!&t%3Asubmit=%5B%22submit_1%22%2C%22submit_0%22%5D&t%3Azoneid=identification'
