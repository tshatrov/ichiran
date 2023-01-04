set -ex

wget ${ICHIRAN_DB_URL} --quiet -O ~/ichiran.pgdump
createdb -E 'UTF8' -l 'ja_JP.utf8' -T template0 dummy

set +ex

pg_restore -C -d dummy ~/ichiran.pgdump
echo "========================="
echo "Finished ichiran DB init!"
echo "========================="