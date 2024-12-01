-- Function to read the content of a file at a given path
on readUTF8File(hfsPath)
	try
		-- Convert HFS path to POSIX path
		set posixPath to POSIX path of hfsPath
		-- Convert POSIX path to a file alias
		set fileReference to (POSIX file posixPath) as alias
		-- Read the file content as UTF-8
		set fileContent to read fileReference as «class utf8»
		return fileContent
	on error errorMessage
		return "Error: " & errorMessage
	end try
end readUTF8File


on run {input, parameters}
	set filePath to item 1 of input
	set fileContent to readUTF8File(filePath)
	return fileContent
end run

