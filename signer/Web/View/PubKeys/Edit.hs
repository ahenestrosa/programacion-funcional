module Web.View.PubKeys.Edit where
import Web.View.Prelude

data EditView = EditView { pubKey :: PubKey }

instance View EditView where
    html EditView { .. } = [hsx|
        {breadcrumb}
        <h1>Edit PubKey</h1>
        {renderForm pubKey}
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "PubKeys" PubKeysAction
                , breadcrumbText "Edit PubKey"
                ]

renderForm :: PubKey -> Html
renderForm pubKey = formFor pubKey [hsx|
    {(textField #pem)}
    {(textField #date)}
    {submitButton}

|]