---
layout: architect
title: Writing a crawler
---

A word before we start.
Writing crawlers is only necessary to get specific data from specific sites. {{ site.name}} already contains a number of pre-made crawlers for common tasks such as downloading an entire site or getting all images.

Crawling works through successor-generating functions that take in one URL, extract
some content, and ouput a list of new URLs to crawl. In their simplest form, we could write them as of type `URL -> [URL]`. We could then write a fetching function as `URL -> (URL -> [URL]) -> Tree URL`. It would take an initial URL, a crawler, and build up a tree of results which could be traversed and saved to disk.

Things are, of course, a bit more complicated than that, but that is the basic idea. The *actual* type of a crawler is

{% highlight haskell %}URL -> ByteString -> a -> [SuccessorNode e a]`{% endhighlight %}
Let us inspect that:

* `URL` is the URL of the current page,
* `ByteString` is its content,
* `a` is the crawler's current state.

In addition to the current URL and the page's contents, the crawler can make use of an arbitrary state that is passed from parent to child. This can be a counter, a predicate, or nothing at all, if it is not needed.

With these inputs, the crawler must return a list `[SuccessorNode e a]`, which contains the nodes that ought to be crawled next.

A successor node is simply a URL and a new state, with a bit of gadgetry added:

{% highlight haskell %}
SuccessorNode e a = {
   nodeState  :: a,
   nodeRes    :: FetchResult e,
   nodeReqMod :: Request -> Request,
   nodeURL    :: URL}
{% endhighlight %}


`nodeState` and `nodeURL` are self-explanatory. `nodeReqMod` is a function that can modify the request via which the next page would be fetched. For example, the crawler could add a HTTP header that modified the referer, or set a cookie.

The core of the node is `nodeRes` - the actual result. A `FetchResult` can be one of the following:

* `Blob`: This indicates that the value of `nodeURL` should be downloaded. This is the result type for pictures, .zip-files, audio, and anything else that should be fetched without looking at it.
* `PlainText`: Simple text, generally extracted from the current site.
* `BinaryData`: Binary data, e.g. some embedded file, or the contents of the current page, if it is already a binary file and not an HTML file.
* `XmlResult`: XML Data. Generally a slice of the current page's DOM.
* `Info`: A pair of key and value.
* `Failure`: Failure nodes indicate that something didn't go as expected. The page might have not have contained some expected content, or it might have shown a "404" message.
* `Inner`: Inner nodes are not counted as results; they are the ones that will be crawled next.

Only `Failure` and `Inner` get any special treatment from the rest of the application. `Blob`, `PlainText`, and friends exist only for the benefit of the user, serving to indicate the type of content that was downloaded. We can thus partition results into three categories:

* Non-failure leaf nodes: anything that will be saved to disk.
* Failure nodes: fetch processes might retry these, or insert them, e.g. in the case of network errors.
* Inner nodes: these tell the fetch process which URLs to get next. Inner nodes aren't deemed "useful" once the crawling is done and are consequently not saved.

Convenience functions
--------------------

Of course, one cannot expect a crawler-writer to remember all of this mumbo-jumbo. Luckily, there are a couple of helper functions that expedite the writing of crawlers:

