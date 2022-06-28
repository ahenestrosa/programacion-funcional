module Web.View.Messages.Show where
import Web.View.Prelude

data ShowView = ShowView { message :: Message, signature :: Text, signature2 :: Text}

instance View ShowView where
    html ShowView { .. } = [hsx|
        {breadcrumb}
        <p>{message}</p>
        <p> {signature} </p>
        <p> {signature2} </p>

    |]
        where
            breadcrumb = renderBreadcrumb
                            [ breadcrumbText "Show Message"
                            ]