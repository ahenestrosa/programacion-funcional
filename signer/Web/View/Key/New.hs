module Web.View.Key.New where
import Web.View.Prelude

data NewView = NewView { key :: Key }

instance View NewView where
    html NewView { .. } = [hsx|
        {breadcrumb}
        <h1>New Key</h1>
        {renderForm key}
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Keys" KeysAction
                , breadcrumbText "New Key"
                ]

renderForm :: Key -> Html
renderForm key = formFor key [hsx|
    {(textField #pem)}
    {(textField #date)}
    {submitButton}

|]