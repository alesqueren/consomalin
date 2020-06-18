curl -X POST -c cookies.txt localhost:3000/users/register --data 'username="a@a.a"&password="a"'
curl -X POST -c cookies.txt localhost:3000/users/login --data 'username="a@a.a&password=a"'

curl -X GET -b cookies.txt localhost:3000/users/me

curl -X GET -b cookies.txt localhost:3000/wishlist 2> /dev/null | python -m json.tool
curl -X POST -b cookies.txt localhost:3000/wishlist/groups/
curl -X POST -b cookies.txt localhost:3000/wishlist/groups/ -d 'name="test groupe"'

curl -X PUT -b cookies.txt localhost:3000/wishlist/groups/f952842087ba4dfb3baf5c60104550f91e3ad18c3ff3127d3399c8ca16abd1cc/ -d 'name="test wish"'
curl -X POST -b cookies.txt localhost:3000/wishlist/groups/f952842087ba4dfb3baf5c60104550f91e3ad18c3ff3127d3399c8ca16abd1cc/wishes/bulk -d 'names=["w1","w2"]'


MONGO

db.user.update({"_id": "a@a.a"}, {"wishGroups": []})
