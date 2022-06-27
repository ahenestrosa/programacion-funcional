module Web.View.Key.Edit where
import Web.View.Prelude

data EditView = EditView { key :: Key }

instance View EditView where
    html EditView { .. } = [hsx|
        {breadcrumb}
        <h1>Edit Key</h1>
        {renderForm key}
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Keys" KeysAction
                , breadcrumbText "Edit Key"
                ]

renderForm :: Key -> Html
renderForm key = formFor key [hsx|
    {(textField #pem)}
    {(textField #date)}
    {submitButton}

|]