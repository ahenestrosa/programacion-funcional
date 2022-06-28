module Web.View.PubKeys.Show where
import Web.View.Prelude

data ShowView = ShowView { pubKey :: PubKey }

instance View ShowView where
    html ShowView { .. } = [hsx|
        {breadcrumb}
        <h1>Show PubKey</h1>
        <p>{pubKey}</p>

    |]
        where
            breadcrumb = renderBreadcrumb
                            [ breadcrumbLink "PubKeys" PubKeysAction
                            , breadcrumbText "Show PubKey"
                            ]