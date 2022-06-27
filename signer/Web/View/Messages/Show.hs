module Web.View.Messages.Show where
import Web.View.Prelude

data ShowView = ShowView { message :: Message, signature :: String, signature2 :: String,result :: Bool}

instance View ShowView where
    html ShowView { .. } = [hsx|
        {breadcrumb}
        <p>{message}</p>
        <p> {signature} </p>
        <p> {signature2} </p>
        <p> {result} </p>

    |]
        where
            breadcrumb = renderBreadcrumb
                            [ breadcrumbText "Show Message"
                            ]