module Web.View.Key.Show where
import Web.View.Prelude

data ShowView = ShowView { key :: Key }

instance View ShowView where
    html ShowView { .. } = [hsx|
        {breadcrumb}
        <h1>Show Key</h1>
        <p>{key}</p>

    |]
        where
            breadcrumb = renderBreadcrumb
                            [ breadcrumbLink "Keys" KeysAction
                            , breadcrumbText "Show Key"
                            ]