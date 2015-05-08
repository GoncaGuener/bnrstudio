# Date: May 7, 2015
# File: API_calls_from_r.r
#
# This script enables a Nerve Center user to make API calls directly from R, 
# and to convert the resulting json into a data frame. 
# Only users with API access will be able to execute these functions. 
# Please contact support@bottlenose.com if you wish to gain API access. 

# If you do not have httr and/or rjson install, do so by executing these commands:
# install.paclages("rjson")
# install.packages("httr")

library(httr)
library(rjson)

BuildUrl <- function(rootURL, userId, userToken, streamId, limit, to, from){
	# Build a full Nerve Center API URL.
	#
	# Args:
	# 	rootURL: for the Activities it is: http://streams.bottlenose.com/3/activities.
	#		   	 for the Metrics call it is: http://streams.bottlenose.com/3/metrics.
	#		     for the Entities call it is: http://streams.bottlenose.com/3/entities.
	# 	userID: The Organization ID listed under the Developer tab found in Stream Settings.
	# 	userToken: The Organization Token listed under the Developer tab found in Stream Settings.
	# 	userToken: The Organization Token listed under the Developer tab found in Stream Settings.
	# 	streamID: The Stream ID listed under the Developer tab found in Stream Settings.
	#	limit: if provided, this is the maximum number of returned messages.
	#	to: if provided, this is the upper bound on a time window. 
	#	from: if provided, this is the lower bound on a time window. 
	#
	# Returns:
	# 	A full http URL path used to make a Nerve Center API call

	if(missing(rootURL))
		stop("Must supply the root API URL.  For example, for an activities call, 
			use: http://streams.bottlenose.com/3/activities")
	if(missing(userId))
		stop("Must supply a userID. This is the Organization ID listed under 
			the Developer tab found in Stream Settings")
	if(missing(userToken))
		stop("Must supply a userToken. This is the Organization Token listed 
			under the Developer tab found in Stream Settings")
	if(missing(streamId))
		stop("Must supply the streamID.  This is the Stream ID listed under 
			the Developer tab found in Stream Settings")
	URL<-paste(rootURL, "?", "&userId=", userId, "&userToken=", userToken, "&streamId=", streamId, sep="")
	if(missing(limit)==FALSE){
		URL<-paste(URL, "&limit=", limit, sep="")
	}
	if(missing(to)==FALSE){
		URL<-paste(URL, "&to=", to, sep="")
	}
	if(missing(from)==FALSE){
		URL<-paste(URL, "&from=", from, sep="")
	}
	return (URL)
}

MakeCall <- function(URL){
	# Makes a Nerve Center API call and formats the output as json
	# Args:
	#	A complete Nerve Center URL, which is generated from the BuildUrl function
	#
	# Returns: 
	# 	Response fields as a list of json fields

	response <- GET(URL)
	json <- content(response, as = "text")
	content <-fromJSON(json)
	return (content)
}

BuildDataFrame<-function(content){
	# Converts a list of json fields into a Data Frame.
	#
	# Args: content, which is the output from the MakeCall function. 
	#
	# Returns: As is, this returns a 5 column data frame containing 
	# published date, source (twitter, facebook, or tumblr), 
	# user (the user name, not user id), message (the actual content of 
	# the tweet, FB post, or tumble post, but note that FB posts do not 
	# always have a "content" field), and sentiment (from -20 to +20). 
	#
	# Note that this function should be changed if the user wants different
	# columns in the resulting data frame. 

	number_of_results<-length(content$result)

	df <- data.frame(matrix(vector(), 
		number_of_results, # the number of messages to include in the data frame
		5, # number of columns
		dimnames=list(c(), c("published_date", "source", "user", "message", "sentiment"))),
		stringsAsFactors=F)

	for (i in 1:number_of_results){
		if(is.null(content$result[[i]]$published)==TRUE){
			df$published_date[i]<-"NULL"
		} else {
			df$published_date[i]<-content$result[[i]]$published
		}
		if(is.null(content$result[[i]]$provider$id)==TRUE){
			df$source[i]<-"NULL"
		} else {
			df$source[i]<-content$result[[i]]$provider$id
		}
		if (is.null(content$result[[i]]$actor$displayName)==TRUE){
			df$user[i]<-"NULL"
		} else {
			df$user[i]<-content$result[[i]]$actor$displayName
		}
		if (is.null(content$result[[i]]$object$content)==TRUE){
			df$message[i]<-"NULL"
		} else {
			df$message[i]<-content$result[[i]]$object$content
		}
		if (is.null(content$result[[i]]$meta$sentiment)==TRUE){
			df$sentiment[i]<-"NULL"
		} else {
			df$sentiment[i]<-content$result[[i]]$meta$sentiment
		}
	}

	return (df)
}


# Building a data frame for the Allianz Stream
url.Allianz <- BuildUrl(
	rootURL="http://streams.bottlenose.com/3/activities", 
	userId="54bec14fd8f683e893e61570",
	userToken="6fcccf0960a053946f1575de08abceee66c3c1029990ea8aba20e30c9eb3506dd8843380cca64d51b8d14e00f4ad5539ab0a65f996527d2443859b26cb8c4549", 
	streamId="551c520cbecb4125ac9513ae",
	limit="1000"
	)
content.Allianz<-MakeCall(url.Allianz)
df.Allianz<-BuildDataFrame(content.Allianz)

# Building a data frame for the ERGO stream
url.ERGO <- BuildUrl(
	rootURL="http://streams.bottlenose.com/3/activities", 
	userId="54bec14fd8f683e893e61570",
	userToken="6fcccf0960a053946f1575de08abceee66c3c1029990ea8aba20e30c9eb3506dd8843380cca64d51b8d14e00f4ad5539ab0a65f996527d2443859b26cb8c4549", 
	streamId="551c5215d71c8d0aacfb64bd",
	limit="1000"
	)
content.ERGO<-MakeCall(url.ERGO)
df.ERGO<-BuildDataFrame(content.ERGO)

# Building a data frame for the Firemans Fund stream
url.Firemans.Fund <- BuildUrl(
	rootURL="http://streams.bottlenose.com/3/activities", 
	userId="54bec14fd8f683e893e61570",
	userToken="6fcccf0960a053946f1575de08abceee66c3c1029990ea8aba20e30c9eb3506dd8843380cca64d51b8d14e00f4ad5539ab0a65f996527d2443859b26cb8c4549", 
	streamId="552cfc97d6de8e14b70ecf68",
	limit="1000"
	)
content.Firemans.Fund <-MakeCall(url.Firemans.Fund)
df.Firemans.Fund <-BuildDataFrame(content.Firemans.Fund)

# Building a data frame for the Talanx stream
url.Talanx <- BuildUrl(
	rootURL="http://streams.bottlenose.com/3/activities", 
	userId="54bec14fd8f683e893e61570",
	userToken="6fcccf0960a053946f1575de08abceee66c3c1029990ea8aba20e30c9eb3506dd8843380cca64d51b8d14e00f4ad5539ab0a65f996527d2443859b26cb8c4549", 
	streamId="551952fad71c8d0aacfb6489",
	limit="1000"
	)
content.Talanx <-MakeCall(url.Talanx)
df.Talanx <-BuildDataFrame(content.Talanx)

# Some extra documentation:

# Example calls using the from, to, and limit options
#	url <- build_url(
#		root_URL="http://streams.bottlenose.com/3/activities", 
#		userId="51399024564946904d000018",
#		userToken="02e9488d38aebcd99d6b6e22143d3ffdcfe98298", 
#		streamId="515bb0da9235e8e06a000009",
#		from="1399852800000",
#		to="1400457600000",
#		)

# The API response fields are not yet fully document, below are some potentially interesting fields
# that you could add to the BuildDataFrame function. 
	#Results
		# $result[[i]]
			# $published - the longform published
			# $publishedTs -- the time-series representation in millisecond time
			# $provider$id -- the source of the content (either facebook, tumblr, or twitter)
			# $meta$klout -- klout score of the user
			# $meta$lang -- the language, with "en" being english
			# $meta$topics[[i]]$w -- a topic extracted from the message
			# $meta$topics[[i]]$s -- our confidence in the topic being an actual topic
			# $meta$sentiment -- the sentiment score for that message
			# $object$content -- the body of the tweet/fb post/tumblr message
			# $actor$followerCount -- # of followers of the author
			# $actor$displayName -- actor's handle name