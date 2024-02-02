echo "========================="
echo "Starting ichiran DB init!"
echo "========================="

createdb -E 'UTF8' -l 'ja_JP.utf8' -T template0 jmdict
set +e
pg_restore -d jmdict /ichiran.pgdump --no-owner --no-privileges

echo "========================="
echo "Finished ichiran DB init!"
echo "========================="