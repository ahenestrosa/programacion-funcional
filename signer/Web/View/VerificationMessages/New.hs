module Web.View.VerificationMessages.New where
import Web.View.Prelude

data NewView = NewView { verificationMessage :: VerificationMessage }

instance View NewView where
    html NewView { .. } = [hsx|
        {breadcrumb}
        <h1>New VerificationMessage</h1>
        {renderForm verificationMessage}
    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbText "New VerificationMessage"
                ]

renderForm :: VerificationMessage -> Html
renderForm verificationMessage = formFor verificationMessage [hsx|
    {(textField #text)}
    {(textField #signature)}
    {(textField #date)}
    {submitButton}

|]