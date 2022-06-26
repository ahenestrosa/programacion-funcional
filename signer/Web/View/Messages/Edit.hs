module Web.View.Messages.Edit where
import Web.View.Prelude

data EditView = EditView { message :: Message }

instance View EditView where
    html EditView { .. } = [hsx|
        {breadcrumb}
        <h1>Edit Message</h1>
        {renderForm message}
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Messages" MessagesAction
                , breadcrumbText "Edit Message"
                ]

renderForm :: Message -> Html
renderForm message = formFor message [hsx|
    {(textField #text)}
    {submitButton}

|]