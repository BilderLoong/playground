-- Function to read the content of a file at a given path
on readUTF8File(hfsPath)
	try
		-- Convert HFS path to POSIX path
		set posixPath to POSIX path of hfsPath
		-- Convert POSIX path to a file alias
		set fileReference to (POSIX file posixPath) as alias
		-- Read the file content as UTF-8
		set fileContent to read fileReference as Çclass utf8È
		return fileContent
	on error errorMessage
		return "Error: " & errorMessage
	end try
end readUTF8File



set hfsfilePath to ":Macintosh HD/Users/birudo/Library/Containers/com.ameba.TRex/Data/tmp/shortcutInput"

set posixPath to POSIX path of hfsfilePath

set fileContent to readUTF8File(posixPath)

-- Display results
-- set output to (fileContent as text)
display dialog fileContent