module Web.View.Messages.Show where
import Web.View.Prelude

data ShowView = ShowView { text :: Text, signature :: Text}

instance View ShowView where
    html ShowView { .. } = [hsx|
        {breadcrumb}
        <p>{text}</p>
        <p> {signature} </p>

    |]
        where
            breadcrumb = renderBreadcrumb
                            [ breadcrumbText "Show Message"
                            ]