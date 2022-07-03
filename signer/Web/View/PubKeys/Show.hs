module Web.View.PubKeys.Show where
import Web.View.Prelude

data ShowView = ShowView { pubKey :: PubKey }

instance View ShowView where
    html ShowView { .. } = [hsx|
        {breadcrumb}
        <h1>Show Public Key</h1>
        <p><b> PEM of public key: </b></p>
        <p>{get #pem pubKey}</p>
        <br/>
        <p><b> Date of public key:&nbsp;&nbsp; </b> {get #date pubKey}</p>

    |]
        where
            breadcrumb = renderBreadcrumb
                            [breadcrumbLink "Index" IndexAction, 
                             breadcrumbLink "PubKeys" PubKeysAction,
                             breadcrumbText "Show PubKey"
                            ]