DownloadStatus url@Text bytes@Int total@Int percent@Int: Downloading #{url}: #{percent}%, #{bytes}/#{total}B.
Downloaded url@Text: #{url} downloaded.

# crawlers
MaxNumOfPages: Enter max. num of pages to get (or leave blank): 
DirAsker: Enter direction ('F' for forwards (default), 'B' for backwards): 
TransformChoice: What should be done to the output?
TransformOptions: Options 
TransformDefault: (default) 
TransformRangeErr min@Int max@Int: Expected a number between #{min} and #{max}!
EnterNumItems: Enter number of items to get: 
EnterURL: Enter URL: 
ExpectedNonEmpty: Expected a non-empty string!

FileListCrawlerDesc: downloads a list of numbered files.
FileCrawlerDesc: downloads a single file.
ImagesCrawlerDesc: downloads all linked images from a page.
FileTypesCrawlerDesc: downloads all files of a given type.
FileTypesCrawlerTag example@Text: Enter list of tags/attribtues\ne.g. #{example}: 
FileTypesCrawlerTagErr: Expected a list of string pairs!
FileTypesCrawlerExt example@Text: Enter list of allowed file extensions\ne.g. #{example}: 
FileTypesCrawlerExtErr: Expected a list of file extensions!
XPathCrawlerDesc: gets all URLs returned by an XPath-expression.
XPathCrawlerPath: Enter XPath-expression:

# Generic error messages
FileDoesNotExist: File does not exist!
FileDoesNotExist1 filename@Text: The file '#{filename}' does not exist!
RenameNoPermission: No permission to rename the file!
RenameNoPermission1 filename@Text: No permission to rename the file '#{filename}'!
RenameIllegalOperation: Can't rename the file - the given path is invalid!
RenameIllegalOperation1 filename@Text: Can't rename the file - '#{filename}' is an invalid path!
FullError: Can't write - the device is full!
FullError1 filename@Text: Can't write '#{filename}' - the device is full!
SomeIOError ex_str@String: IO exception: #{ex_str}
CrawlerDoesNotExist: No crawler by that name!
OtherError ex_str@String: Some other error: #{ex_str}
InvalidPath: Invalid path (incorrect format or no write permission)!
MetaNoFile filename@Text: The file '#{filename}' mentioned in the metadata file does not exist!
MetaNoParse: Couldn't parse the metadata file!
ExpectedPosNum: Expected a positive integer!
ExpectedDir: Expected F/B!
URLHasNoNum: The URL did not contain any number!

# Hephaestos's own exceptions
HTMLParsingErr url@Text: Couldn't parse the content of '#{url}' as HTML!
MetadataParsingErr filename@Text: Couldn't parse '#{filename}' metadata file!
URIParsingErr url@Text: Couldn't parse '#{url}' as a URL!
DataMissingErr url@Text missingData@Text: Expected element '#{missingData}' not found in '#{url}'!
DataMissingErr1 url@Text: Expected element not found in '#{url}'!
DataFormatErr url@Text: Incorrectly formated data in '#{url}'!
DomainCrossedErr dom@Text url@Text: The URL '#{url}' was outside of the intended domain '#{dom}'!
DuplicateFileErr source@Text target@Text: Can't rename '#{source}' to '#{target}'. The target already exists!
DuplicateFileErr1 target@Text: Can't rename file to '#{target}'. The target already exists!
AmbiguousDataErr: Ambiguous data found!
AmbiguousKeyErr key@Text: Ambiguous data: more than one key called '#{key}' found!
DataMissingMetaErr: A file mentioned in the metadata file doesn't exist!

# HTTP exceptions
StatusCodeErr code@Int msg@Text: Status #{code} #{msg}!
InvalidUrlErr url@Text: '#{url}' is an invalid URL!
TooManyRedirectsErr: Too many redirects!
UnparseableRedirectErr: Unparseable redirect!
TooManyRetriesErr: Too many retries!
HttpParserErr: Couldn't parse HTTP!
HandshakeFailedErr: Handshake failed!
OverlongHeadersErr: Overlong headers!
ResponseTimeoutErr: Response timeout!
FailedConnectionErr host@Text port@Int: Failed connecting to #{host}:#{port}!
ExpectedBlankAfter100ContinueErr: Expected blank after 100 Continue!
InvalidStatusLineErr line@Text: Invalid status line #{line}!
InvalidHeaderErr header@Text: Invalid header '#{header}'!
InternalIOErr: Internal IO exception!
ProxyConnectErr: Proxy connection exception!
NoResponseDataReceivedErr: Empty response!
TlsErr ex@Text: "TLS exception: #{ex}
TlsNotSupportedErr: TLS not supported!
ResponseBodyTooShortErr expected@Int got@Int: Response body too short. Expected #{expected}, got #{got}!"
InvalidChunkHeadersErr: Invalid chunk headers!
IncompleteHeadersErr: Incomplete headers!
InvalidDestinationHostErr host@Text: Invalid destination host '#{host}'!"
HttpZlibErr: Zlib exception!

# transformations
NameByURL: name by URL: get the filenames from the URLs.
StructureByURL: structure by URL: organize according to URL structure.
StructureByKey: structure by key: organize according to "title" keys.
TransID: do nothing.

#CLI
NoOpC: Does nothing.
UnknownC: Unknown command.
ExitC: Exits the program.
HelpC: Prints this help text.
ChangeDirC: Changes the currrent directory.
ChangeDirAsk: Enter new directory: 
PrintDirC: Prints the current directory.
CrawlerC: Runs a crawler.
ListC: Lists all available crawlers.
TransC: Runs a transformation on downloaded files.
HelpTitle version@Text: Hephaestos v#{version}.
HelpDesc: CLI Interface. Run crawlers.
CurrentDir dir@Text: Current download folder: #{dir}.
JobDone: Job done.
Quitting: Quitting...
TasksFinished num@Int: Successfully finished downloads: #{num}
TasksRunning num@Int: Running downloads: #{num}
TasksFailed num@Int: Failed downloads: #{num}
TasksOmitted num@Int: #{num} others...

TransEnterName: Name of the metadata file:
CrawlerEnterName altCmd@Text: The crawler's name (or type '#{altCmd}' to list available ones): 
UnknownCommand cmd@Text: Unknown command '#{cmd}'. Type :[h]elp for a list of available commands or ':[e]xit'.

# config
NoParseErr filename@Text: Couldn't parse the configuration file '#{filename}'! Correct the file or delete it to restore defaults. If this message persists, the application has a bug.

