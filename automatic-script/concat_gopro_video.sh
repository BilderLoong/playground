read -p "Please the last three number of videos that you want to concat: " n

for v in {1..5} 
do
	if [ ${v} -eq 1 ]; then
		echo file \'GX0${v}0${n}.mp4\' > temp.txt
	else
		echo file \'GX0${v}0${n}.mp4\' >> temp.txt
	fi
done
echo File list:

cat temp.txt

echo run ffmpeg!

ffmpeg -f concat -i temp.txt -c copy $n.mp4
