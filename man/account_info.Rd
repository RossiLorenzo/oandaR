% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/account_info.R
\name{account_info}
\alias{account_info}
\title{Get account information}
\usage{
account_info(token, accountId, accountType = c("Trade", "Practice"))
}
\arguments{
\item{token}{The personal access token. There is a link on your OANDA fxTrade account profile page titled 'Manage API Access' (My Account -> My Services -> Manage API Access). From there, you can generate a personal access token to use with the OANDA API, as well as revoke a token you may currently have.}

\item{accountId}{The account id to fetch the list of info for. Can be retrieved with a call of \code{account_list}.}

\item{accountType}{The subdomain for the request is dependent on the environment you wish to obtain access tokens for. Possible choices are Practice or Trade}
}
\description{
Get details for one of the accounts owned by the user
}
\examples{
account_info('my_token_1234', 'my_account_1234')
}

