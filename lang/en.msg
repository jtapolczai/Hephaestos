# @messages/en.msg
DownloadStatus url@Text bytes@Int total@Int percent@Int: Downloading #{url}: #{percent}%, #{bytes}/#{total}B.
Downloaded url@Text: #{url} downloaded.
UnknownC: Unknown command.
ExitC: Exits the program.
HelpC: Prints this help text.
CDC: Changes the currrent directory.
PrintDirC: Prints the current directory.
CrawlerC: Runs a crawler.
ListC: Lists all available crawlers.
TransC: Runs a transformation on downloaded files.

TransEnterName: Name of the metadata file:
CrawlerEnterName altCmd@Text: Enter crawler name (or type '#{altCmd}' to list available ones):

HelpTitle version@Text: Hephaestos v#{version}.
HelpDesc: CLI Interface. Run crawlers.
CurrentDir dir@Text: Current download folder: #{dir}.
JobDone: Job done.
Quitting: Quitting...

# crawlers
MaxNumOfPages: Enter max. num of pages to get (or leave blank):
ForwardsDir: Forwards
BackwardsDir: Backwards


# error messages
UnknownCommand cmd@Text: Unknown command '#{cmd}'. Type :[h]elp for a list of available commands or ':[e]xit'.
FileDoesNotExist: File does not exist.
CrawlerDoesNotExist: No crawler by that name.
InvalidPath: Invalid path (incorrect format or no write permission).
AmbiguousKey: More than one key found!
MetaNoFile filename@Text: The file '#{filename}' mentioned in the metadata file does not exist!
MetaNoParse: Couldn't parse the metadata file!
ExpectedPosNum: Expected a positive integer!
ExpectedDir forwards@Text backwards@Text: Expected #{forwards}/#{backwards}!
URLHasNoNum: The URL did not contain any number!
NoURIParse link@Text: Couldn't parse '#{link}' as a URI!

HTMLParsingErr: Couldn't parse content as HTML!
DataMissingErr url@Text missingData@Text: Expected element '#{missingData}' not found in '#{url}'!
DataMissingErr1 url@Text: Expected element not found in '#{url}'!
DataFormatErr url@Text: Incorrectly formated data in '#{url}'!
DomainCrossedErr dom@Text url@Text: The URL '#{url}' was outside of the intended domain '#{dom}'!
DuplicateFileErr source@Text target@Text: Can't rename '#{source}' to '#{target}'. The target already exists!
DuplicateFileErr1 target@Text: Can't rename file to '#{target}'. The target already exists!

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

# config
NoParseErr filename@Text: Couldn't parse the configuration file '#{filename}'! Correct the file or delete it to restore defaults. If this message persists, the application has a bug.
