module Web.View.Messages.Show where
import Web.View.Prelude

data ShowView = ShowView { signature :: Text, date :: Day}

instance View ShowView where
    html ShowView { .. } = [hsx|
        {breadcrumb}
        <h1>Sigature for File</h1>

        <p> {signature} </p>
        <p> {date} </p>

    |]
        where
            breadcrumb = renderBreadcrumb
                [   breadcrumbLink "Index" IndexAction,
                    breadcrumbLink "New Message Signature" NewMessageAction,
                    breadcrumbText "Show Message Signaure"
                ]