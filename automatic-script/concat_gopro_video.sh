read -p "Please the number of videos that you want to concat: " n



# https://stackoverflow.com/questions/7333232/how-to-concatenate-two-mp4-files-using-ffmpeg
file_list=$(find . -type f -name 'GX*'${n}'.MP4' -printf "file '%p'\n" | sort) 

echo $file_list

echo run ffmpeg!

ffmpeg -f concat -i <$file_list -c copy $n.mp4
