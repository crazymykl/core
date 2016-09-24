var _elm_lang$core$Native_Http = function() {


// ENCODING AND DECODING URI

function encodeUri(string)
{
	return encodeURIComponent(string);
}

function decodeUri(string)
{
	return decodeURIComponent(string);
}


return {
	encodeUri: encodeUri,
	decodeUri: decodeUri
};

}();
