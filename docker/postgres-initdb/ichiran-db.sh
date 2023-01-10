echo "========================="
echo "Starting ichiran DB init!"
echo "========================="

createdb -E 'UTF8' -l 'ja_JP.utf8' -T template0 dummy
set +e
pg_restore -C -d dummy /ichiran.pgdump

echo "========================="
echo "Finished ichiran DB init!"
echo "========================="