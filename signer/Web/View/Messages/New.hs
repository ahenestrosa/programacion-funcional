module Web.View.Messages.New where
import Web.View.Prelude

data NewView = NewView { message :: Message }

instance View NewView where
    html NewView { .. } = [hsx|
        {breadcrumb}
        <h1>New Message</h1>
        {renderForm message}
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Messages" MessagesAction
                , breadcrumbText "New Message"
                ]

renderForm :: Message -> Html
renderForm message = formFor message [hsx|
    {(textField #text)}
    {submitButton}

|]