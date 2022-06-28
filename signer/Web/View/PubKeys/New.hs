module Web.View.PubKeys.New where
import Web.View.Prelude

data NewView = NewView { pubKey :: PubKey }

instance View NewView where
    html NewView { .. } = [hsx|
        {breadcrumb}
        <h1>New PubKey</h1>
        {renderForm pubKey}
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "PubKeys" PubKeysAction
                , breadcrumbText "New PubKey"
                ]

renderForm :: PubKey -> Html
renderForm pubKey = formFor pubKey [hsx|
    {(textField #pem)}
    {(textField #date)}
    {submitButton}

|]