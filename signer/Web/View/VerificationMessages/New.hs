module Web.View.VerificationMessages.New where
import Web.View.Prelude

data NewView = NewView {}

instance View NewView where
    html NewView {} = [hsx|
        {breadcrumb}
        <h1>New VerificationMessage</h1>

        <form method="POST" action="/CreateVerificationMessage" id="" class="new-form">
            <div class="form-group" id="form-group-message_text">
                <label for="message_signature">Text</label>
                <input type="text" name="signature" id="message_signature" class="form-control" />
            </div>

            <div class="form-group" id="form-group-message_file">
                <input    
                    type="file"
                    name="file"
                    accept="application/pdf"
                >
            </div>


            <button class="btn btn-primary">Create Message</button>
        </form>

    |]
        where
            breadcrumb = renderBreadcrumb
                [ breadcrumbText "New VerificationMessage"
                ]



-- renderForm :: VerificationMessage -> Html
-- renderForm verificationMessage = formFor verificationMessage [hsx|
--     {(textField #text)}
--     {(textField #signature)}
--     {(textField #date)}
--     {submitButton}

-- |]