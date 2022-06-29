module Web.View.Messages.Show where
import Web.View.Prelude

data ShowView = ShowView { signature :: Text, date :: Day}

instance View ShowView where
    html ShowView { .. } = [hsx|
        {breadcrumb}
        <p> {signature} </p>
        <p> {date} </p>

    |]
        where
            breadcrumb = renderBreadcrumb
                            [ breadcrumbText "Show Message"
                            ]