module Web.View.VerificationMessages.New where
import Web.View.Prelude

data NewView = NewView {}

instance View NewView where
    html NewView {} = [hsx|
        {breadcrumb}
        <h1>Verify Signature for File</h1>

        <form method="POST" action="/CreateVerificationMessage" id="" class="new-form">
            <div class="form-group" id="form-group-message_text">
                <label for="message_signature">Signature</label>
                <input type="text" name="signature" id="message_signature" class="form-control" />
            </div>

            <div class="form-group" id="form-group-message_file">
                <label for="file">File to Verify </label>
                <br/>
                <input    
                    type="file"
                    name="file"
                    accept="application/pdf, image/jpg, image/png, text/plain"
                >
            </div>

            <div class="form-group" id="form-group-message_created_at">
                <label for="date">Created At</label>
                <input type="date" name="date" id="date" class="form-control" />
            </div>

            <button class="btn btn-primary">Verify</button>
        </form>

    |]
        where
            breadcrumb = renderBreadcrumb
                [   breadcrumbLink "Index" IndexAction,
                    breadcrumbText "Verify Message Signature"
                ]



-- renderForm :: VerificationMessage -> Html
-- renderForm verificationMessage = formFor verificationMessage [hsx|
--     {(textField #text)}
--     {(textField #signature)}
--     {(textField #date)}
--     {submitButton}

-- |]