/**
 * Register a URL change handler that redirects twitter.com links to nitter.net
 */

chrome.webRequest.onBeforeRequest.addListener(function(details) {
  let url = new URL(details.url);
  return {
    redirectUrl: ('https://nitter.net' + url.pathname)
  };
}, {urls: ['*://twitter.com/*'], types: ['main_frame']}, ['blocking']);
